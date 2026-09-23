000100 01  MOD2-W9O33502.                                                       
000200*                                 MODCOPYTEXT TILL W9033500               
000300*                                 WORK WITH ORDER                         
000400*                                 FRÅGA ARTIKEL/ORDER.                    
000500*                                                                         
000600*                                 FLSVAR KAN INNEHÅLLA                    
000700*                                    = LÄS FRÅN BÖRJAN PÅ WDQ4            
000800*                                  O = LÄS NÄSTA ORDER PÅ WDQ4            
000900*                                  P = LÄS NÄSTA ORDER PÅ WDE4            
001000*                                  R = LÄS NÄSTA ORDER PÅ WDA5            
001100*                                 ANVÄNDS VID BLÄDDRING                   
001200     03 MOD2-IDTRANS         PIC X(4).                                    
001300*                                 BILDNUMMER                              
001400     03 MOD2-IDMFSFEL        PIC X(3).                                    
001500*                                 MFS FELMEDDELANDE NUMMER                
001600     03 MOD2-IDARTNR         PIC X(9).                                    
001700*                                 ARTIKELNUMMER                           
001800     03 MOD2-IDORDNR7-NEXT   PIC 9(7).                                    
001900*                                 ORDERNUMMER                             
002000     03 MOD2-IDDC-NEXT       PIC X(2).                                    
002100*                                 IDENTIFIERARE LAGER                     
002200     03 MOD2-IDORDER-NEXT    PIC 9(7).                                    
002300*                                 VOLVO PARTS ORDERNUMMER                 
002400     03 MOD2-ADLAGOMR-NEXT   PIC 9(2).                                    
002500*                                 LAGEROMRÅDE                             
002600     03 MOD2-ADGANG-NEXT     PIC 9(2).                                    
002700*                                 GÅNG                                    
002800     03 MOD2-ADPLATS-NEXT    PIC 9(5).                                    
002900*                                 LAGERPLATSNUMMER                        
003000     03 MOD2-IDPRODNR-NEXT   PIC 9(7).                                    
003100*                                 PRODUKTIONSNUMMER                       
003200     03 MOD2-IDKOLLI-NEXT    PIC 9(5).                                    
003300*                                 KOLLINUMMER                             
003400     03 MOD2-IDRADNR-NEXT    PIC 9(4).                                    
003500*                                 RADNUMMER                               
003600     03 MOD2-FLSVAR-NEXT     PIC X.                                       
003700*                                 ALLMÄN SVARSFLAGGA                      
003800     03 MOD2-RAD             OCCURS 13 TIMES.                             
003900        05 MOD2-IDORDNR7-RAD PIC 9(7).                                    
004000*                                 ORDERNUMMER                             
004100        05 MOD2-TIDISPIN     PIC 9(6).                                    
004200*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
004300        05 MOD2-KVBEART-Q    PIC 9(7).                                    
004400*                                 BESTÄLLT KVANTANPASSAT ANTAL            
004500        05 MOD2-KVPREAVB     PIC 9(7).                                    
004600*                                 PREL-AVB KVANT                          
004700        05 MOD2-IDDC-RAD     PIC X(2).                                    
004800*                                 IDENTIFIERARE LAGER                     
004900        05 MOD2-KDORDSTA     PIC X(2).                                    
005000*                                 VOLVOORDERSTATUS                        
005100        05 MOD2-IDORDNR7-LEV PIC 9(7).                                    
005200*                                 ORDERNUMMER                             
005300        05 MOD2-TIANNULL     PIC 9(6).                                    
005400*                                 ANNULLATIONSDATUM (ÅÅMMDD)              
005500        05 MOD2-KDORDKL      PIC 9.                                       
005600*                                 ORDERKLASS                              
005700        05 MOD2-IDRADINF-GRP.                                             
005800*                                 RADINFO                                 
005900           07 MOD2-IDRADINF  PIC X(13).                                   
006000*                                 RADINFO                                 
006100           07 MOD2-IDBIL REDEFINES MOD2-IDRADINF.                         
006200*                                 BILIDENTITET                            
006300              09 MOD2-IDBILTYP                                            
006400                             PIC X(3).                                    
006500*                                 BILTYP                                  
006600              09 MOD2-TIAAAA PIC X(4).                                    
006700*                                 ÅRTAL (ÅÅÅÅ)                            
006800              09 MOD2-IDCHASSI-PIE                                        
006900                             PIC X(6).                                    
007000*                                 CHASSINUMMER PIE                        
007100           07 MOD2-IDLEVINF REDEFINES MOD2-IDRADINF.                      
007200*                                 LEV INFO                                
007300              09 MOD2-IDLEVNMN                                            
007400                             PIC X(10).                                   
007500*                                 LEVERANTÖRENS NAMN                      
007600              09 MOD2-IDTECKEN                                            
007700                             PIC X.                                       
007800*                                 TECKEN                                  
007900              09 MOD2-KVDAGAR-DIFF                                        
008000                             PIC X(2).                                    
008100*                                 TRANSPORT DAY DIFF. (+/- CONTRA         
008200*                                  CDC)                                   
008300        05 MOD2-KDANNULL     PIC X.                                       
008400*                                 CANCELLATION CODE                       
008500*** END OF VILMAII-COPY LENGTH= 825 BYTES                                 
