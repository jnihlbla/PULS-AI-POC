000100 01  W011041.                                                             
000200*                                 KORTTYP R05  TILL W011                  
000300*                                 FÖR SPECIALBEHANDLING                   
000400*                                                                         
000500     03 IDPTYP               PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 IDARTNR              PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900     03 KDCLAGER             PIC S9              COMP-3.                  
001000*                                 CENTRALLAGERKOD                         
001100     03 IDELMT               PIC X(16).                                   
001200*                                 DATAELEMENTIDENTITET                    
001300     03 FLKNTRL              PIC X.                                       
001400*                                 FLAGGA DATAKONTROLL                     
001500     03 KDFUNC               PIC X.                                       
001600*                                 MOVE/ADD                                
001700     03 IDFVARDE-NYTT        PIC X(25).                                   
001800*                                 FÄLTVÄRDE                               
001900     03 IDFVARDE-BEF         PIC X(25).                                   
002000*                                 FÄLTVÄRDE                               
002100*** END COPY W011041CC0  LENGTH=77                                        
