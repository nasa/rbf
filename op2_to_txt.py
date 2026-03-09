#!/usr/bin/env python
"""
op2_to_txt.py - Extract NASTRAN SOL 103 mode shapes from OP2 to Tecplot format.

Reads a NASTRAN OP2 file containing SOL 103 eigenvector results and writes:
  - modeNNN.txt  : Plain text (x, y, z, f1-f6), no headers. Used for rbf input.
  - modeNNN.dat  : Tecplot ASCII format with mesh connectivity for plotting
  - freq.txt     : Modal frequencies (mode, radians, cycles)
"""

__version__ = '1.0.0'
__date__    = '2026-03-09'
__author__  = 'Steve Massey'
__email__   = 'steven.j.massey@nasa.gov'
__org__     = 'NASA Langley Research Center'

# Sun Mar  9 2026: Updated element type handling for broader compatibility

import argparse
import numpy as np
from collections import Counter
from pyNastran.op2.op2_geom import OP2Geom


def get_element_connectivity(op2, node_id_to_index):
    """
    Extract element connectivity from OP2 and organize by Tecplot element type.
    
    Returns dict with keys 'TRIANGLE', 'QUADRILATERAL', 'LINESEG', 'TETRAHEDRON', 'BRICK'
    Each value is a list of element connectivities (0-based indices).
    """
    
    elements = {
        'LINESEG': [],       # 2-node elements
        'TRIANGLE': [],      # 3-node elements
        'QUADRILATERAL': [], # 4-node elements
        'TETRAHEDRON': [],   # 4-node volume elements
        'BRICK': [],         # 8-node volume elements
    }
    
    # Diagnostic: count all element types in the model
    type_counts = Counter()
    skipped_types = Counter()
    
    for eid, elem in op2.elements.items():
        elem_type = elem.type
        type_counts[elem_type] += 1
        
        try:
            # --- 2-node line elements -> LINESEG ---
            if elem_type in ['CBAR', 'CBEAM', 'CROD', 'CONROD', 'CTUBE',
                             'CBEND', 'CVISC', 'CDAMP1', 'CDAMP2',
                             'CELAS1', 'CELAS2', 'CGAP']:
                if hasattr(elem, 'nodes') and len(elem.nodes) >= 2:
                    nids = elem.nodes[:2]
                    conn = [node_id_to_index.get(nid) for nid in nids]
                    if None not in conn:
                        elements['LINESEG'].append(conn)
                    else:
                        skipped_types[elem_type] += 1
                else:
                    skipped_types[elem_type] += 1
                    
            # --- 3-node triangle elements -> TRIANGLE ---
            elif elem_type in ['CTRIA3', 'CTRIAR']:
                nids = elem.nodes[:3]
                conn = [node_id_to_index.get(nid) for nid in nids]
                if None not in conn:
                    elements['TRIANGLE'].append(conn)
                else:
                    skipped_types[elem_type] += 1
                    
            elif elem_type in ['CTRIA6']:
                # Higher-order triangle -> use corner nodes only
                nids = elem.nodes[:3]
                conn = [node_id_to_index.get(nid) for nid in nids]
                if None not in conn:
                    elements['TRIANGLE'].append(conn)
                else:
                    skipped_types[elem_type] += 1
                    
            # --- 4-node quad elements -> QUADRILATERAL ---
            elif elem_type in ['CQUAD4', 'CSHEAR', 'CQUADR']:
                nids = elem.nodes[:4]
                conn = [node_id_to_index.get(nid) for nid in nids]
                if None not in conn:
                    elements['QUADRILATERAL'].append(conn)
                else:
                    skipped_types[elem_type] += 1
                    
            elif elem_type in ['CQUAD8', 'CQUAD']:
                # Higher-order quad -> use corner nodes only
                nids = elem.nodes[:4]
                conn = [node_id_to_index.get(nid) for nid in nids]
                if None not in conn:
                    elements['QUADRILATERAL'].append(conn)
                else:
                    skipped_types[elem_type] += 1
                    
            # --- 4-node tet elements -> TETRAHEDRON ---
            elif elem_type in ['CTETRA']:
                nids = elem.nodes[:4]
                conn = [node_id_to_index.get(nid) for nid in nids]
                if None not in conn:
                    elements['TETRAHEDRON'].append(conn)
                else:
                    skipped_types[elem_type] += 1
                    
            # --- 8-node hex elements -> BRICK ---
            elif elem_type in ['CHEXA']:
                nids = elem.nodes[:8]
                conn = [node_id_to_index.get(nid) for nid in nids]
                if None not in conn:
                    elements['BRICK'].append(conn)
                else:
                    skipped_types[elem_type] += 1
                    
            # --- 6-node wedge -> degenerate BRICK (repeat last 2 nodes) ---
            elif elem_type in ['CPENTA']:
                nids = elem.nodes[:6]
                conn = [node_id_to_index.get(nid) for nid in nids]
                if None not in conn:
                    # Map 6-node wedge to 8-node brick:
                    # wedge nodes: 0,1,2,3,4,5
                    # brick nodes: 0,1,2,2,3,4,5,5 (degenerate)
                    brick_conn = [conn[0], conn[1], conn[2], conn[2],
                                  conn[3], conn[4], conn[5], conn[5]]
                    elements['BRICK'].append(brick_conn)
                else:
                    skipped_types[elem_type] += 1
                    
            # --- 5-node pyramid -> degenerate BRICK ---
            elif elem_type in ['CPYRAM']:
                nids = elem.nodes[:5]
                conn = [node_id_to_index.get(nid) for nid in nids]
                if None not in conn:
                    # Map 5-node pyramid to 8-node brick:
                    # pyramid: 0,1,2,3,4(apex)
                    # brick:   0,1,2,3,4,4,4,4
                    brick_conn = [conn[0], conn[1], conn[2], conn[3],
                                  conn[4], conn[4], conn[4], conn[4]]
                    elements['BRICK'].append(brick_conn)
                else:
                    skipped_types[elem_type] += 1
                    
            # --- Elements with no useful connectivity for visualization ---
            elif elem_type in ['CBUSH', 'CBUSH1D', 'CBUSH2D',
                               'CELAS3', 'CELAS4',
                               'CDAMP3', 'CDAMP4',
                               'CMASS1', 'CMASS2', 'CMASS3', 'CMASS4',
                               'CONM1', 'CONM2',
                               'RBE1', 'RBE2', 'RBE3', 'RBAR', 'RROD',
                               'RJOINT', 'RTRPLT',
                               'PLOTEL', 'SPOINT', 'EPOINT',
                               'CDUM1', 'CDUM2', 'CDUM3', 'CDUM4',
                               'CDUM5', 'CDUM6', 'CDUM7', 'CDUM8', 'CDUM9']:
                # These don't produce useful mesh visualization
                skipped_types[elem_type] += 1
                
            else:
                # Unknown element type - try to handle generically
                skipped_types[elem_type] += 1
                
        except (AttributeError, IndexError, TypeError) as e:
            skipped_types[elem_type] += 1
    
    # --- Diagnostic report ---
    print("\nAll element types in OP2:")
    for etype, count in sorted(type_counts.items(), key=lambda x: -x[1]):
        print(f"  {etype:16s}: {count:6d}")
    
    print("\nTecplot-compatible elements extracted:")
    any_found = False
    for etype, elist in elements.items():
        if elist:
            print(f"  {etype:16s}: {len(elist):6d} elements")
            any_found = True
    
    if not any_found:
        print("  (none)")
    
    if skipped_types:
        print("\nSkipped element types (not mesh-compatible or missing nodes):")
        for etype, count in sorted(skipped_types.items(), key=lambda x: -x[1]):
            print(f"  {etype:16s}: {count:6d}")
    
    return elements


def write_frequencies(op2, freq_filename='freq.txt'):
    """
    Write modal frequencies to a text file.
    """
    with open(freq_filename, 'w') as f:
        if op2.eigenvectors:
            for subcase_id, eigvec in op2.eigenvectors.items():
                f.write(f"{'MODE':<10} {'RADIANS':<20} {'CYCLES':<20}\n")

                modes = eigvec.modes

                # Get frequencies
                if hasattr(eigvec, 'mode_cycles'):
                    freqs = eigvec.mode_cycles
                elif hasattr(eigvec, 'freqs'):
                    freqs = eigvec.freqs
                else:
                    freqs = [None] * len(modes)

                # Write data
                for i, mode in enumerate(modes):
                    if i < len(freqs):
                        freq = freqs[i]
                        if isinstance(freq, (int, float)):
                            f.write(f"{mode:<10} {freq*2*np.pi:<20.6f} {freq:<20.6f}\n")
                        else:
                            f.write(f"{mode:<10} {freq*2*np.pi:<20.6f} {str(freq):<20}\n")

                f.write("\n")
        else:
            f.write("No modal data found in .op2 file\n")
    
    print(f"Modal frequencies extracted to '{freq_filename}'")

def write_modeshapes_to_tecplot(op2_file, scale_factor=1.0):
    """
    Read NASTRAN OP2 file and write mode shapes to Tecplot format.
    Supports multi-zone output for mixed element types.
    Orphan nodes (not in any element) are written as a separate point zone.
    """
    
    print(f"Reading OP2 file: {op2_file}")
    
    op2 = OP2Geom()
    op2.read_op2(op2_file)
    
    # Get eigenvectors
    if not op2.eigenvectors:
        print("ERROR: No eigenvectors found in OP2 file")
        return
    
    subcase_id = list(op2.eigenvectors.keys())[0]
    eigenvectors = op2.eigenvectors[subcase_id]
    
    modes = eigenvectors.modes
    num_modes = len(modes)
    print(f"Found {num_modes} modes in subcase {subcase_id}")
    
    # Get node coordinates from OP2 geometry
    if not op2.nodes:
        print("ERROR: No nodes found in OP2 geometry")
        return
    
    print(f"Found {len(op2.nodes)} nodes in OP2 geometry")
    print(f"Found {len(op2.elements)} elements in OP2 geometry")
    
    # Build node arrays - match eigenvector node order
    eig_node_ids = eigenvectors.node_gridtype[:, 0]
    
    # Filter to only GRID points (gridtype == 1)
    grid_mask = eigenvectors.node_gridtype[:, 1] == 1
    node_ids = eig_node_ids[grid_mask]
    
    num_nodes = len(node_ids)
    coords = np.zeros((num_nodes, 3))
    
    # Build node_id -> index mapping
    node_id_to_index = {}
    for i, nid in enumerate(node_ids):
        node_id_to_index[nid] = i
        if nid in op2.nodes:
            node = op2.nodes[nid]
            coords[i, :] = node.xyz
    
    # Get element connectivity
    elements = get_element_connectivity(op2, node_id_to_index)
    
    # Determine which Tecplot element types have data
    priority_order = ['QUADRILATERAL', 'TRIANGLE', 'BRICK', 'TETRAHEDRON', 'LINESEG']
    tecplot_types = []
    for etype in priority_order:
        if elements[etype]:
            tecplot_types.append(etype)
    
    # Find primary element type (most numerous)
    primary_etype = None
    max_count = 0
    for etype in priority_order:
        count = len(elements[etype])
        if count > max_count:
            max_count = count
            primary_etype = etype
    
    if not tecplot_types:
        print("\nWarning: No compatible elements found. Writing point data only.")
    else:
        print(f"\nPrimary element type for Tecplot: {primary_etype} ({max_count} elements)")
        if len(tecplot_types) > 1:
            print(f"Additional types: {', '.join(t for t in tecplot_types if t != primary_etype)}")
    
    # =========================================================================
    # Identify orphan nodes (GRID nodes not referenced by any element)
    # =========================================================================
    nodes_in_elements = set()
    for etype in priority_order:
        for conn in elements[etype]:
            for idx in conn:
                nodes_in_elements.add(idx)
    
    orphan_indices = sorted(set(range(num_nodes)) - nodes_in_elements)
    num_orphans = len(orphan_indices)
    
    print(f"\nNodes in elements:    {len(nodes_in_elements)}")
    print(f"Orphan nodes:         {num_orphans}")
    print(f"Total GRID nodes:     {num_nodes}")
    
    # =========================================================================
    # Write mode shape files
    # =========================================================================
    print(f"\nWriting {num_modes} mode files...")
    
    for mode_idx, mode_num in enumerate(modes):
        dat_filename = f'mode{mode_num:03d}.dat'
        txt_filename = f'mode{mode_num:03d}.txt'
        
        # Get frequency
        try:
            if hasattr(eigenvectors, 'cycles') and eigenvectors.cycles is not None:
                freq = eigenvectors.cycles[mode_idx]
            elif hasattr(eigenvectors, 'eigns') and eigenvectors.eigns is not None:
                freq = np.sqrt(abs(eigenvectors.eigns[mode_idx])) / (2 * np.pi)
            elif hasattr(eigenvectors, 'eigrs') and eigenvectors.eigrs is not None:
                freq = np.sqrt(abs(eigenvectors.eigrs[mode_idx])) / (2 * np.pi)
            else:
                freq = 0.0
        except:
            freq = 0.0
        
        # Get displacement data for this mode
        disp_data_full = eigenvectors.data[mode_idx, :, :]
        disp_data = disp_data_full[grid_mask, :]
        
        # Write Tecplot .dat file with connectivity
        with open(dat_filename, 'w') as f:
            f.write(f'TITLE = "NASTRAN SOL 103 Mode {mode_num} - {freq:.4f} Hz"\n')
            f.write('VARIABLES = "x", "y", "z", "f1", "f2", "f3", "f4", "f5", "f6"\n')
            
            if tecplot_types:
                # ---------------------------------------------------------
                # FE zones for each element type
                # ---------------------------------------------------------
                for zone_idx, etype in enumerate(tecplot_types):
                    elem_list = elements[etype]
                    num_elems = len(elem_list)
                    
                    if zone_idx == 0:
                        # First zone: owns the node data
                        f.write(f'ZONE T="Mode {mode_num} {etype} - {freq:.4f} Hz", ')
                        f.write(f'N={num_nodes}, E={num_elems}, ')
                        f.write(f'F=FEPOINT, ET={etype}\n')
                    else:
                        # Subsequent zones: share node data from zone 1
                        f.write(f'ZONE T="Mode {mode_num} {etype} - {freq:.4f} Hz", ')
                        f.write(f'N={num_nodes}, E={num_elems}, ')
                        f.write(f'F=FEPOINT, ET={etype}, ')
                        f.write(f'VARSHARELIST=([1-9]=1)\n')
                    
                    # Write node data only for first zone
                    if zone_idx == 0:
                        for i in range(num_nodes):
                            x, y, z = coords[i, :]
                            f1 = disp_data[i, 0] * scale_factor
                            f2 = disp_data[i, 1] * scale_factor
                            f3 = disp_data[i, 2] * scale_factor
                            f4 = disp_data[i, 3] * scale_factor
                            f5 = disp_data[i, 4] * scale_factor
                            f6 = disp_data[i, 5] * scale_factor
                            
                            f.write(f'{x:17.9e} {y:17.9e} {z:17.9e} ')
                            f.write(f'{f1:17.9e} {f2:17.9e} {f3:17.9e} ')
                            f.write(f'{f4:17.9e} {f5:17.9e} {f6:17.9e}\n')
                    
                    # Write element connectivity (1-based for Tecplot)
                    for conn in elem_list:
                        conn_1based = [idx + 1 for idx in conn]
                        f.write(' '.join(f'{n:8d}' for n in conn_1based) + '\n')
                
                # ---------------------------------------------------------
                # Orphan node zone (scatter points)
                # ---------------------------------------------------------
                if num_orphans > 0:
                    f.write(f'ZONE T="Mode {mode_num} Orphan Nodes - {freq:.4f} Hz", ')
                    f.write(f'I={num_orphans}, F=POINT\n')
                    
                    for i in orphan_indices:
                        x, y, z = coords[i, :]
                        f1 = disp_data[i, 0] * scale_factor
                        f2 = disp_data[i, 1] * scale_factor
                        f3 = disp_data[i, 2] * scale_factor
                        f4 = disp_data[i, 3] * scale_factor
                        f5 = disp_data[i, 4] * scale_factor
                        f6 = disp_data[i, 5] * scale_factor
                        
                        f.write(f'{x:17.9e} {y:17.9e} {z:17.9e} ')
                        f.write(f'{f1:17.9e} {f2:17.9e} {f3:17.9e} ')
                        f.write(f'{f4:17.9e} {f5:17.9e} {f6:17.9e}\n')
            
            else:
                # ---------------------------------------------------------
                # No elements at all: write ALL nodes as point data
                # ---------------------------------------------------------
                f.write(f'ZONE T="Mode {mode_num} - {freq:.4f} Hz", ')
                f.write(f'I={num_nodes}, F=POINT\n')
                
                for i in range(num_nodes):
                    x, y, z = coords[i, :]
                    f1 = disp_data[i, 0] * scale_factor
                    f2 = disp_data[i, 1] * scale_factor
                    f3 = disp_data[i, 2] * scale_factor
                    f4 = disp_data[i, 3] * scale_factor
                    f5 = disp_data[i, 4] * scale_factor
                    f6 = disp_data[i, 5] * scale_factor
                    
                    f.write(f'{x:17.9e} {y:17.9e} {z:17.9e} ')
                    f.write(f'{f1:17.9e} {f2:17.9e} {f3:17.9e} ')
                    f.write(f'{f4:17.9e} {f5:17.9e} {f6:17.9e}\n')
        
        # Write simple .txt file (no headers, all nodes)
        with open(txt_filename, 'w') as f:
            for i in range(num_nodes):
                x, y, z = coords[i, :]
                f1 = disp_data[i, 0] * scale_factor
                f2 = disp_data[i, 1] * scale_factor
                f3 = disp_data[i, 2] * scale_factor
                f4 = disp_data[i, 3] * scale_factor
                f5 = disp_data[i, 4] * scale_factor
                f6 = disp_data[i, 5] * scale_factor
                
                f.write(f'{x:17.9e} {y:17.9e} {z:17.9e} ')
                f.write(f'{f1:17.9e} {f2:17.9e} {f3:17.9e} ')
                f.write(f'{f4:17.9e} {f5:17.9e} {f6:17.9e}\n')
        
        print(f"  Wrote {dat_filename}, {txt_filename} - Mode {mode_num}: {freq:.4f} Hz")
    
    # Write frequency file
    write_frequencies(op2)
    
    # Print summary
    print("\n" + "=" * 50)
    print("Mode Summary:")
    print("=" * 50)
    for mode_idx, mode_num in enumerate(modes):
        try:
            if hasattr(eigenvectors, 'cycles') and eigenvectors.cycles is not None:
                freq = eigenvectors.cycles[mode_idx]
            elif hasattr(eigenvectors, 'eigns') and eigenvectors.eigns is not None:
                freq = np.sqrt(abs(eigenvectors.eigns[mode_idx])) / (2 * np.pi)
            else:
                freq = 0.0
        except:
            freq = 0.0
        print(f"  mode{mode_num:03d}.dat/.txt  -  Mode {mode_num:3d}: {freq:12.4f} Hz")
    
    print(f"\nSuccessfully wrote {num_modes} mode files ({num_modes * 2} total files) + freq.txt")
    return modes


# =============================================================================
# MAIN EXECUTION
# =============================================================================
if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description='Extract NASTRAN SOL 103 mode shapes to Tecplot format with mesh connectivity'
    )
    parser.add_argument('--version', action='version',
                        version=f'%(prog)s {__version__} ({__date__})')
    parser.add_argument('op2_file', help='Path to the NASTRAN OP2 file')
    parser.add_argument('--scale', type=float, default=1.0, 
                        help='Scale factor for displacements (default: 1.0)') 

    args = parser.parse_args()
    
    # Run the conversion
    write_modeshapes_to_tecplot(args.op2_file, scale_factor=args.scale)
