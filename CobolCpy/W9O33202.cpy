000100 01  MOD2-W9O33202.                                                       
000200*                                 MODCOPYTEXT TILL W9033200               
000300*                                 ORDER INQUIRY.                          
000400*                                 FRÅGA ORDER ELLER ARTIKEL.              
000500     03 MOD2-IDTRANS         PIC X(4).                                    
000600*                                 BILDNUMMER                              
000700     03 MOD2-IDMFSFEL        PIC X(3).                                    
000800*                                 MFS FELMEDDELANDE NUMMER                
000900     03 MOD2-IDORDNR7        PIC X(7).                                    
001000*                                 ORDERNUMMER                             
001100     03 MOD2-IDARTNR         PIC X(8).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 MOD2-IDRADNR-NEXT    PIC X(4).                                    
001400*                                 RADNUMMER                               
001500     03 MOD2-IDPRODNR-NEXT   PIC X(7).                                    
001600*                                 PRODUKTIONSNUMMER                       
001700     03 MOD2-IDPLKLST-NEXT   PIC X(3).                                    
001800*                                 PLOCKLISTNUMMER                         
001900     03 MOD2-IDKOLLI-NEXT    PIC X(5).                                    
002000*                                 KOLLINUMMER                             
002100     03 MOD2-IDDC-NEXT       PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 MOD2-FLSVAR-NEXT     PIC X.                                       
002400*                                 ALLMÄN SVARSFLAGGA                      
002500     03 MOD2-RAD             OCCURS 11 TIMES.                             
002600        05 MOD2-IDARTNR-RAD  PIC 9(8).                                    
002700*                                 ARTIKELNUMMER                           
002800        05 MOD2-KVBEART      PIC 9(6).                                    
002900*                                 BESTÄLLT ANTAL STYCKEN                  
003000        05 MOD2-KVAVBART     PIC 9(6).                                    
003100*                                 AVBOKAT ANTAL ARTIKLAR                  
003200        05 MOD2-KVLEVART     PIC 9(6).                                    
003300*                                 LEVERERAT ANTAL STYCK                   
003400        05 MOD2-IDDC         PIC X(2).                                    
003500*                                 IDENTIFIERARE LAGER                     
003600        05 MOD2-KDORDSTA     PIC X(2).                                    
003700*                                 VOLVOORDERSTATUS                        
003800        05 MOD2-IDKOLLI      PIC 9(5).                                    
003900*                                 KOLLINUMMER                             
004000        05 MOD2-KDTPOTYP     PIC X.                                       
004100*                                 TYP AV TIDPLANERAD ORDER                
004200        05 MOD2-TIRODAT      PIC 9(6).                                    
004300*                                 RESTORDERDATUM         (ÅÅMMDD)         
004400        05 MOD2-IDORDNR7-RO  PIC 9(7).                                    
004500*                                 ORDERNUMMER                             
004600        05 MOD2-IDRADINF-GRP.                                             
004700*                                 RADINFO                                 
004800           07 MOD2-IDRADINF  PIC X(13).                                   
004900*                                 RADINFO                                 
005000           07 MOD2-IDBIL REDEFINES MOD2-IDRADINF.                         
005100*                                 BILIDENTITET                            
005200              09 MOD2-IDBILTYP                                            
005300                             PIC X(3).                                    
005400*                                 BILTYP                                  
005500              09 MOD2-TIAAAA PIC X(4).                                    
005600*                                 ÅRTAL (ÅÅÅÅ)                            
005700              09 MOD2-IDCHASSI-PIE                                        
005800                             PIC X(6).                                    
005900*                                 CHASSINUMMER PIE                        
006000           07 MOD2-IDLEVINF REDEFINES MOD2-IDRADINF.                      
006100*                                 LEV INFO                                
006200              09 MOD2-IDLEVNMN                                            
006300                             PIC X(10).                                   
006400*                                 LEVERANTÖRENS NAMN                      
006500              09 MOD2-IDTECKEN                                            
006600                             PIC X.                                       
006700*                                 TECKEN                                  
006800              09 MOD2-KVDAGAR-DIFF                                        
006900                             PIC X(2).                                    
007000*                                 TRANSPORT DAY DIFF. (+/- CONTRA         
007100*                                  CDC)                                   
007200*** END OF VILMAII-COPY LENGTH= 726 BYTES                                 
