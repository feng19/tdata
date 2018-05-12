#!/usr/bin/env python
# -*- coding: utf-8 -*-
import xlrd
from erlport import erlterms
from erlport.erlterms import Atom, encode_term

def load_excel(excel_name):
    data = xlrd.open_workbook(excel_name) # open file
    sheets = data.sheets()

    sheets_res = []
    header_len = 0
    for table in sheets:
        sheet_name = table.name
        nrows = table.nrows
        is_first = True
        rows = []
        for i in range(nrows):
            cells = table.row_values(i)
            row_line = i + 1
            if is_first:
                header_len = get_header_len(cells)
                if header_len==0:
                    print "error ! %s's %s first row is all empty"%(sheet_name, excel_name)
                    break
                is_first = False
                rows.append(format_number(row_line, cells[:header_len]))
                continue
            cells = cells[:header_len]
            if is_all_empty(cells):
                continue
            rows.append(format_number(row_line, cells))
        sheets_res.append((sheet_name.encode("utf-8"), rows))
    return sheets_res

def get_header_len(cells):
    header_len = 0
    for cell in cells:
        if len(cell)==0:
            break
        header_len = header_len +1
    return header_len

def is_all_empty(cells):
    for cell in cells:
        if cell<>'':
            return False
    return True

def format_number(row_line, cells):
    res = []
    column = 1
    for cell in cells:
        cell_type = type(cell)
        if ( cell_type == float ):
            if ( cell == int(cell) ):
                res.append((row_line, column, Atom('int'), int(cell)))
                column = column + 1
                continue
        elif ( cell_type == unicode ):
            res.append((row_line, column, Atom('binary'), cell.encode("utf-8")))
            column = column + 1
            continue
        res.append((row_line, column, format_type(cell_type, cell), cell))
        column = column + 1
    return res

def format_type(cell_type, cell):
    if ( cell_type == bool ): return Atom('bool')
    elif ( cell_type == int ): return Atom('int')
    elif ( cell_type == long ): return Atom('long')
    elif ( cell_type == float ): return Atom('float')
    elif ( cell_type == str ): return Atom('binary')
    elif ( cell_type == tuple ): return Atom('tuple')
    elif ( cell_type == list ): return Atom('list')
    elif ( cell_type == dict ): return Atom('dict')
    elif ( cell_type == bytes ): return Atom('binary')
    else: return Atom('undefined')
