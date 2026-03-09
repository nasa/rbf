#!/usr/bin/env python
#!/usr/bin/env python
"""
plot_modes.py - Visualize NASTRAN SOL 103 mode shapes from Tecplot .dat files.

Reads the modeNNN.dat files produced by op2_to_txt.py and creates
interactive 3D plots with deformed mesh colored by displacement magnitude.
Supports static images, PNG export, and animated MP4/GIF output.
"""

__version__ = '1.0.0'
__date__    = '2026-03-09'
__author__  = 'Steve Massey'
__email__   = 'steven.j.massey@nasa.gov'
__org__     = 'NASA Langley Research Center'

import argparse
import glob
import os
import re
import sys
import numpy as np

try:
    import pyvista as pv
except ImportError:
    print("ERROR: pyvista is required. Install with:")
    print("  pip install pyvista")
    print("  # or for headless/remote:")
    print("  pip install 'pyvista[jupyter]'")
    sys.exit(1)


def parse_tecplot_dat(filename):
    """
    Parse a Tecplot ASCII .dat file with FEPOINT or POINT format.
    Returns a list of zone dicts, each with:
        'title': zone title string
        'nodes': (N,9) array [x,y,z,f1,f2,f3,f4,f5,f6]
        'elements': list of connectivity arrays (0-based) or None
        'etype': Tecplot element type string or None
        'format': 'FEPOINT' or 'POINT'
    """
    zones = []
    
    with open(filename, 'r') as f:
        lines = f.readlines()
    
    i = 0
    file_title = ""
    
    while i < len(lines):
        line = lines[i].strip()
        
        # Skip empty lines
        if not line:
            i += 1
            continue
        
        # File TITLE
        if line.upper().startswith('TITLE'):
            file_title = line.split('=', 1)[1].strip().strip('"')
            i += 1
            continue
        
        # VARIABLES line
        if line.upper().startswith('VARIABLES'):
            i += 1
            continue
        
        # ZONE line (may span multiple lines)
        if line.upper().startswith('ZONE'):
            zone_header = line
            # Collect continuation lines (no keyword start)
            while i + 1 < len(lines):
                next_line = lines[i + 1].strip()
                if (next_line and 
                    not next_line.upper().startswith('ZONE') and
                    not next_line.upper().startswith('TITLE') and
                    not next_line.upper().startswith('VARIABLES') and
                    not next_line[0].lstrip('-').replace('.','',1).replace('E','',1).replace('e','',1).replace('+','',1).isdigit()):
                    zone_header += ' ' + next_line
                    i += 1
                else:
                    break
            i += 1
            
            # Parse zone header
            zh = zone_header.upper()
            
            # Extract zone title
            zt_match = re.search(r'T\s*=\s*"([^"]*)"', zone_header, re.IGNORECASE)
            zone_title = zt_match.group(1) if zt_match else "Zone"
            
            # Check for VARSHARELIST (shared data zone - no node data)
            is_shared = 'VARSHARELIST' in zh
            
            # Determine format
            if 'F=FEPOINT' in zh or 'F = FEPOINT' in zh:
                zone_format = 'FEPOINT'
            else:
                zone_format = 'POINT'
            
            # Get counts
            n_match = re.search(r'\bN\s*=\s*(\d+)', zh)
            e_match = re.search(r'\bE\s*=\s*(\d+)', zh)
            i_match = re.search(r'\bI\s*=\s*(\d+)', zh)
            et_match = re.search(r'ET\s*=\s*(\w+)', zh)
            
            num_nodes = int(n_match.group(1)) if n_match else 0
            num_elems = int(e_match.group(1)) if e_match else 0
            num_points = int(i_match.group(1)) if i_match else 0
            etype = et_match.group(1) if et_match else None
            
            # Read node data
            if zone_format == 'FEPOINT' and not is_shared:
                node_data = np.zeros((num_nodes, 9))
                for ni in range(num_nodes):
                    vals = lines[i].split()
                    node_data[ni, :len(vals)] = [float(v) for v in vals[:9]]
                    i += 1
            elif zone_format == 'POINT':
                n = num_points if num_points > 0 else num_nodes
                node_data = np.zeros((n, 9))
                for ni in range(n):
                    vals = lines[i].split()
                    node_data[ni, :len(vals)] = [float(v) for v in vals[:9]]
                    i += 1
            elif is_shared:
                # Use node data from first zone
                node_data = zones[0]['nodes'] if zones else np.zeros((0, 9))
            else:
                node_data = np.zeros((0, 9))
            
            # Read element connectivity
            elem_list = []
            if num_elems > 0:
                for ei in range(num_elems):
                    vals = lines[i].split()
                    conn = [int(v) - 1 for v in vals]  # Convert to 0-based
                    elem_list.append(conn)
                    i += 1
            
            zones.append({
                'title': zone_title,
                'nodes': node_data,
                'elements': elem_list if elem_list else None,
                'etype': etype,
                'format': zone_format,
                'num_nodes_total': num_nodes if num_nodes > 0 else num_points,
            })
            continue
        
        # If we get here, skip unknown line
        i += 1
    
    return file_title, zones


def zones_to_pyvista(zones):
    """
    Convert parsed Tecplot zones to pyvista mesh objects.
    Returns a pyvista MultiBlock containing all zones.
    """
    blocks = pv.MultiBlock()
    
    for zone in zones:
        nodes = zone['nodes']
        coords = nodes[:, 0:3]
        
        if zone['elements'] and len(zone['elements']) > 0:
            # Build unstructured grid from element connectivity
            cells = []
            celltypes = []
            
            etype = zone['etype']
            
            for conn in zone['elements']:
                n = len(conn)
                
                if etype == 'TRIANGLE' or n == 3:
                    cells.append([3] + list(conn))
                    celltypes.append(pv.CellType.TRIANGLE)
                elif etype == 'QUADRILATERAL' or n == 4:
                    # Could be quad or tet - check etype
                    if etype == 'TETRAHEDRON':
                        cells.append([4] + list(conn))
                        celltypes.append(pv.CellType.TETRA)
                    else:
                        cells.append([4] + list(conn))
                        celltypes.append(pv.CellType.QUAD)
                elif etype == 'TETRAHEDRON':
                    cells.append([4] + list(conn[:4]))
                    celltypes.append(pv.CellType.TETRA)
                elif etype == 'BRICK' or n == 8:
                    cells.append([8] + list(conn))
                    celltypes.append(pv.CellType.HEXAHEDRON)
                elif etype == 'LINESEG' or n == 2:
                    cells.append([2] + list(conn))
                    celltypes.append(pv.CellType.LINE)
                else:
                    # Generic polygon
                    cells.append([n] + list(conn))
                    celltypes.append(pv.CellType.POLYGON)
            
            # Flatten cells list
            flat_cells = []
            for c in cells:
                flat_cells.extend(c)
            
            grid = pv.UnstructuredGrid(
                np.array(flat_cells, dtype=np.int64),
                np.array(celltypes, dtype=np.uint8),
                np.array(coords, dtype=np.float64)
            )
            
            # Add displacement data as point arrays
            grid.point_data['f1'] = nodes[:, 3]
            grid.point_data['f2'] = nodes[:, 4]
            grid.point_data['f3'] = nodes[:, 5]
            grid.point_data['f4'] = nodes[:, 6]
            grid.point_data['f5'] = nodes[:, 7]
            grid.point_data['f6'] = nodes[:, 8]
            grid.point_data['disp_mag'] = np.sqrt(
                nodes[:, 3]**2 + nodes[:, 4]**2 + nodes[:, 5]**2
            )
            
            blocks.append(grid, zone['title'])
        
        else:
            # Point cloud (orphan nodes or point-only zone)
            cloud = pv.PolyData(np.array(coords, dtype=np.float64))
            cloud.point_data['f1'] = nodes[:, 3]
            cloud.point_data['f2'] = nodes[:, 4]
            cloud.point_data['f3'] = nodes[:, 5]
            cloud.point_data['f4'] = nodes[:, 6]
            cloud.point_data['f5'] = nodes[:, 7]
            cloud.point_data['f6'] = nodes[:, 8]
            cloud.point_data['disp_mag'] = np.sqrt(
                nodes[:, 3]**2 + nodes[:, 4]**2 + nodes[:, 5]**2
            )
            
            blocks.append(cloud, zone['title'])
    
    return blocks


def plot_mode(dat_filename, scale=10.0, save_png=False, show_edges=True,
              show_undeformed=True):
    """
    Plot a single mode shape from a Tecplot .dat file.
    """
    file_title, zones = parse_tecplot_dat(dat_filename)
    
    if not zones:
        print(f"No zones found in {dat_filename}")
        return
    
    blocks = zones_to_pyvista(zones)
    
    # Set up plotter
    pl = pv.Plotter(title=file_title, window_size=[1920, 1080],
                    off_screen=save_png)
    pl.set_background('white')
    
    for idx, (block, name) in enumerate(zip(blocks, blocks.keys())):
        if block.n_points == 0:
            continue
        
        disp_mag = block.point_data.get('disp_mag')
        
        # Build displacement vector for warping
        disp_vector = np.column_stack([
            block.point_data['f1'],
            block.point_data['f2'],
            block.point_data['f3']
        ])
        block.point_data['displacement'] = disp_vector
        
        is_point_cloud = isinstance(block, pv.PolyData) and block.n_cells == 0
        
        if is_point_cloud:
            # --- Orphan / scatter points ---
            if show_undeformed:
                pl.add_mesh(block, color='gray', point_size=5,
                            render_points_as_spheres=True,
                            opacity=0.3, label=f'{name} (undeformed)')
            warped = block.warp_by_vector('displacement', factor=scale)
            pl.add_mesh(warped, scalars='disp_mag', cmap='turbo',
                        point_size=8, render_points_as_spheres=True,
                        show_scalar_bar=False,
                        label=name) 
        else:
            # --- Mesh zone ---
            if show_undeformed:
                pl.add_mesh(block, color='lightgray', style='wireframe',
                            opacity=0.3, show_edges=False,
                            label=f'{name} (undeformed)')
            
            warped = block.warp_by_vector('displacement', factor=scale)
            pl.add_mesh(warped, scalars='disp_mag', cmap='turbo',
                        show_edges=show_edges, edge_color='gray',
                        show_scalar_bar=False,
                        label=name)
    
    pl.add_scalar_bar(title='Displacement Magnitude', 
                      vertical=True, n_labels=5, fmt='%.2e')
    pl.add_axes()
    pl.add_text(file_title, position='upper_left', font_size=12, color='black')
    
    # Camera
    pl.view_isometric()
    pl.reset_camera()
    
    if save_png:
        png_name = os.path.splitext(dat_filename)[0] + '.png'
        pl.screenshot(png_name, transparent_background=False)
        print(f"  Saved {png_name}")
        pl.close()
    else:
        pl.show()

def animate_mode(dat_filename, scale=10.0, n_frames=60, save_video=False,
                 video_format='mp4', fps=30):
    """
    Animate a mode shape oscillating sinusoidally.
    
    save_video: if True, save to file instead of interactive display
    video_format: 'mp4', 'gif', or 'png_seq'
    fps: frames per second for mp4/gif
    """
    file_title, zones = parse_tecplot_dat(dat_filename)
    
    if not zones:
        print(f"No zones found in {dat_filename}")
        return
    
    blocks = zones_to_pyvista(zones)
    
    # Pre-compute displacement vectors for each block
    block_disps = []
    for block in blocks:
        if block.n_points == 0:
            block_disps.append(None)
            continue
        disp = np.column_stack([
            block.point_data['f1'],
            block.point_data['f2'],
            block.point_data['f3']
        ])
        block_disps.append(disp)
    
    # Determine global scalar range for consistent colorbar
    all_mags = []
    for block in blocks:
        if block.n_points > 0 and 'disp_mag' in block.point_data:
            all_mags.append(block.point_data['disp_mag'].max())
    global_max = max(all_mags) * scale if all_mags else 1.0
    clim = [0, global_max]
    
    # Determine output filename
    base = os.path.splitext(dat_filename)[0]
    if video_format == 'mp4':
        out_name = base + '.mp4'
    elif video_format == 'gif':
        out_name = base + '.gif'
    else:
        out_name = None
    
    pl = pv.Plotter(title=file_title, window_size=[1920, 1080],
                    off_screen=save_video)
    pl.set_background('white')
    
    phases = np.linspace(0, 2 * np.pi, n_frames, endpoint=False)
    
    # Initial plot at phase=0
    mesh_refs = []
    for idx, (block, name) in enumerate(zip(blocks, blocks.keys())):
        if block.n_points == 0 or block_disps[idx] is None:
            mesh_refs.append(None)
            continue
        
        block.point_data['displacement'] = block_disps[idx] * scale
        warped = block.warp_by_vector('displacement', factor=1.0)
        warped.point_data['disp_mag_scaled'] = np.linalg.norm(
            block_disps[idx] * scale, axis=1)
        
        is_point_cloud = isinstance(block, pv.PolyData) and block.n_cells == 0
        
        if is_point_cloud:
            pl.add_mesh(warped, scalars='disp_mag_scaled', cmap='turbo',
                        clim=clim, point_size=8,
                        render_points_as_spheres=True,
                        show_scalar_bar=False)
        else:
            pl.add_mesh(warped, scalars='disp_mag_scaled', cmap='turbo',
                        clim=clim, show_edges=True, edge_color='gray',
                        show_scalar_bar=False)
        
        mesh_refs.append(warped)
    
    pl.add_scalar_bar(title='Displacement Magnitude',
                      vertical=True, n_labels=5, fmt='%.2e')
    pl.add_axes()
    pl.add_text(file_title, position='upper_left', font_size=12, color='black')
    pl.view_isometric()
    pl.reset_camera()
    
    # --- Collect frames or use pyvista GIF writer ---
    if save_video and video_format == 'gif':
        # Use pyvista's built-in GIF writer
        pl.open_gif(out_name)
        
        for frame, phase in enumerate(phases):
            factor = np.sin(phase)
            for idx, (block, name) in enumerate(zip(blocks, blocks.keys())):
                if block.n_points == 0 or block_disps[idx] is None:
                    continue
                if mesh_refs[idx] is None:
                    continue
                base_pts = block.points.copy()
                mesh_refs[idx].points = base_pts + block_disps[idx] * scale * factor
                mesh_refs[idx].point_data['disp_mag_scaled'] = np.linalg.norm(
                    block_disps[idx] * scale * abs(factor), axis=1)
            pl.write_frame()
        
        pl.close()
        print(f"  Saved {out_name}")
    
    elif save_video and video_format == 'mp4':
        # Render frames to numpy arrays, then write MP4 with imageio
        try:
            import imageio.v3 as iio
            has_imageio = True
        except ImportError:
            try:
                import imageio as iio
                has_imageio = True
            except ImportError:
                has_imageio = False
        
        if not has_imageio:
            print("ERROR: imageio is required for MP4 output. Install with:")
            print("  pip install imageio[ffmpeg]")
            pl.close()
            return
        
        print(f"  Rendering {n_frames} frames...")
        frames = []
        
        # Render first frame
        pl.show(auto_close=False, interactive=False)
        
        for frame_idx, phase in enumerate(phases):
            factor = np.sin(phase)
            for idx, (block, name) in enumerate(zip(blocks, blocks.keys())):
                if block.n_points == 0 or block_disps[idx] is None:
                    continue
                if mesh_refs[idx] is None:
                    continue
                base_pts = block.points.copy()
                mesh_refs[idx].points = base_pts + block_disps[idx] * scale * factor
                mesh_refs[idx].point_data['disp_mag_scaled'] = np.linalg.norm(
                    block_disps[idx] * scale * abs(factor), axis=1)
            
            pl.render()
            img = pl.screenshot(return_img=True)
            
            # Ensure dimensions are even (required by H.264 / yuv420p)
            h, w = img.shape[:2]
            h2 = h if h % 2 == 0 else h - 1
            w2 = w if w % 2 == 0 else w - 1
            if h2 != h or w2 != w:
                img = img[:h2, :w2, :]
            
            frames.append(img)
            
            if (frame_idx + 1) % 10 == 0 or frame_idx == 0:
                print(f"    Frame {frame_idx + 1}/{n_frames}")
        
        pl.close()
        
        # Write MP4 with H.264 + yuv420p for macOS + Windows compatibility
        print(f"  Encoding {out_name}...")
        iio.imwrite(
            out_name,
            frames,
            fps=fps,
            codec='libx264',
            pixelformat='yuv420p',
            quality=8,           # CRF-like: 0-10, higher = better
        )
        
        file_size = os.path.getsize(out_name) / (1024 * 1024)
        print(f"  Saved {out_name} ({file_size:.1f} MB, {n_frames} frames, {fps} fps)")
    
    else:
        # Interactive animation using VTK timer callback
        state = {'frame': 0}
        
        def update_animation(caller, event):
            phase = phases[state['frame'] % n_frames]
            factor = np.sin(phase)
            for idx, (block, name) in enumerate(zip(blocks, blocks.keys())):
                if block.n_points == 0 or block_disps[idx] is None:
                    continue
                if mesh_refs[idx] is None:
                    continue
                base_pts = block.points.copy()
                mesh_refs[idx].points = base_pts + block_disps[idx] * scale * factor
                mesh_refs[idx].point_data['disp_mag_scaled'] = np.linalg.norm(
                    block_disps[idx] * scale * abs(factor), axis=1)
            state['frame'] += 1
            pl.render()
        
        pl.show(auto_close=False, interactive=False)
        iren = pl.iren.interactor
        iren.AddObserver('TimerEvent', update_animation)
        iren.CreateRepeatingTimer(int(1000 / fps))
        iren.Start()
        pl.close()


def find_mode_files():
    """Find all modeNNN.dat files in the current directory."""
    files = sorted(glob.glob('mode[0-9][0-9][0-9].dat'))
    return files


def parse_mode_range(mode_str):
    """Parse mode specification: '5', '1-10', '1,3,5', '1-5,8,10-12'"""
    modes = set()
    for part in mode_str.split(','):
        part = part.strip()
        if '-' in part:
            a, b = part.split('-', 1)
            modes.update(range(int(a), int(b) + 1))
        else:
            modes.add(int(part))
    return sorted(modes)


# =============================================================================
# MAIN
# =============================================================================
if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description='Visualize NASTRAN SOL 103 mode shapes from Tecplot .dat files',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s                                    # Interactive viewer, all modes
  %(prog)s --mode 5                           # Show only mode 5
  %(prog)s --mode 1-5                         # Show modes 1 through 5
  %(prog)s --mode 1,3,5,7                     # Show specific modes
  %(prog)s --scale 100                        # Larger deformation scale
  %(prog)s --save                             # Save PNGs for all modes
  %(prog)s --no-edges                         # Hide mesh edges
  %(prog)s --no-undeformed                    # Hide undeformed wireframe

  %(prog)s --animate 3 --scale 50             # Interactive animation of mode 3
  %(prog)s --animate 3 --scale 50 --save-anim                # Save as MP4
  %(prog)s --animate 3 --scale 50 --save-anim --format gif   # Save as GIF
  %(prog)s --animate 3 --save-anim --frames 120 --fps 30     # 4-sec MP4 at 30fps
        """)
    parser.add_argument('--version', action='version',
                        version=f'%(prog)s {__version__} ({__date__})') 
    parser.add_argument('--mode', type=str, default=None,
                        help='Mode number(s): "5", "1-10", "1,3,5" (default: all)')
    parser.add_argument('--scale', type=float, default=10.0,
                        help='Deformation scale factor (default: 10.0)')
    parser.add_argument('--save', action='store_true',
                        help='Save PNG screenshots instead of interactive display')
    parser.add_argument('--no-edges', action='store_true',
                        help='Hide mesh edges')
    parser.add_argument('--no-undeformed', action='store_true',
                        help='Hide undeformed wireframe overlay')
    parser.add_argument('--animate', type=int, default=None, metavar='MODE',
                        help='Animate a single mode oscillation')
    parser.add_argument('--format', choices=['mp4', 'gif'], default='mp4',
                        help='Animation output format (default: mp4)')
    parser.add_argument('--save-anim', action='store_true',
                        help='Save animation to file (use with --animate)')
    parser.add_argument('--fps', type=int, default=30,
                        help='Frames per second for animation (default: 30)')
    parser.add_argument('--frames', type=int, default=60,
                        help='Number of animation frames (default: 60)')
    
    args = parser.parse_args()
    
    # Handle animation mode
    if args.animate is not None:
        dat_file = f'mode{args.animate:03d}.dat'
        if not os.path.isfile(dat_file):
            print(f"ERROR: {dat_file} not found")
            sys.exit(1)
        print(f"Animating {dat_file} with scale={args.scale}, "
              f"frames={args.frames}, fps={args.fps}")
        animate_mode(dat_file, scale=args.scale, n_frames=args.frames,
                     save_video=args.save_anim, video_format=args.format,
                     fps=args.fps)
        sys.exit(0)
    
    # Find mode files
    all_files = find_mode_files()
    if not all_files:
        print("ERROR: No modeNNN.dat files found in current directory")
        sys.exit(1)
    
    # Filter to requested modes
    if args.mode is not None:
        requested = parse_mode_range(args.mode)
        files = [f'mode{m:03d}.dat' for m in requested if os.path.isfile(f'mode{m:03d}.dat')]
        missing = [m for m in requested if not os.path.isfile(f'mode{m:03d}.dat')]
        if missing:
            print(f"Warning: mode files not found for modes: {missing}")
    else:
        files = all_files
    
    if not files:
        print("ERROR: No matching mode files found")
        sys.exit(1)
    
    print(f"Found {len(files)} mode file(s)")
    print(f"Deformation scale: {args.scale}")
    
    for dat_file in files:
        print(f"\nPlotting {dat_file}...")
        plot_mode(dat_file, scale=args.scale, save_png=args.save,
                  show_edges=not args.no_edges,
                  show_undeformed=not args.no_undeformed)
