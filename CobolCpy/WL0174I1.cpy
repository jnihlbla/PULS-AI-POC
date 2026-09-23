000100 01  REQU-WL0174I1.                                                       
000200*                                 REQUEST TO PGM WL0174                   
000300     03 REQU-IDDC-L174       PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-ART-PRINT-GRP.                                               
000600*                                 GRUPPNIVÅ ARTIKEL-PRINT                 
000700        05 REQU-ART-PRINT    OCCURS 10 TIMES.                             
000800*                                 ARTIKEL-PRINT GRUPP                     
000900           07 REQU-IDARTNR-L174                                           
001000                             PIC 9(8).                                    
001100*                                 ARTIKELNUMMER                           
001200           07 REQU-KDINVPRIO-L174                                         
001300                             PIC 9.                                       
001400*                                 INVENTERING PRIORITET                   
001500           07 REQU-KDINVKAT-L174                                          
001600                             PIC 9(2).                                    
001700*                                 INVENTERINGSKATEGORI                    
001800*** END OF VILMAII-COPY LENGTH= 112 BYTES                                 
