000100 01  REQU-W50294I1.                                                       
000200*                                 REQUEST-COPYTEXT FÖR BILD 5294          
000300*                                 VID ANROP FRÅN WEBBEN                   
000400*                                 ACS COUNT UPDATE                        
000500     03 REQU-IDDC            PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700*                                 WAREHOUSE IDENTIFIER                    
000800     03 REQU-IDACSNR         PIC 9(6).                                    
000900*                                 NR.SERIE FÖR ACS-LISTOR                 
001000*                                 SERIAL NO. FOR ACS REPORTS              
001100     03 REQU-IDCOUNTER-REG   PIC X(20).                                   
001200*                                 REG. AV RÄKNING VID INVENTERING         
001300*                                 PERSON TO REG.COUNT                     
001400     03 REQU-KDACS           PIC X.                                       
001500*                                 KÖRNINGSVARIANT FÖR ACS-RUTIN           
001600*                                 PROCESSING VARIANT FOR ACS RTN          
001700     03 REQU-KVRADER         PIC 9(5).                                    
001800*                                 ANTAL RADER                             
001900*                                 NUMBER OF LINES                         
002000     03 REQU-TABELLRAD       OCCURS 100 TIMES.                            
002100*                                 GROUP WITH LINES TO UPDATE INV          
002200        05 REQU-IDARTNR      PIC 9(8).                                    
002300*                                 ARTIKELNUMMER                           
002400*                                 PART NUMBER                             
002500        05 REQU-KVCOUNT      PIC X(8).                                    
002600*                                 ANTAL TRÄFFAR                           
002700*                                 NUMBER OF HITS                          
002800*** END OF VILMAII-COPY LENGTH= 1634 BYTES                                
