000010*** EDIT ALLOWED                                                          
000100 01  WWLEV05.                                                             
000200*                                                                         
000300*    SUPPLIER RESTRICTIONS REGARDING USE OF PRODUCT-GROUP.                
000400*                                                                         
000500     03  WS-SUPPGRP.                                                      
000600         05  FILLER                PIC X(08)  VALUE 'CWZYA'.              
000800         05  FILLER                PIC 9(02)  VALUE 25.                   
000600         05  FILLER                PIC X(08)  VALUE 'MWAJB'.              
000800         05  FILLER                PIC 9(02)  VALUE 18.                   
003400     03  TAB-SUPPGRP REDEFINES WS-SUPPGRP.                                
003500         05  FILLER  OCCURS 2.                                            
003700             07  TAB-KDARBTYP-LEV  PIC X(08).                             
003700             07  TAB-KDPRODSL      PIC 9(02).                             
