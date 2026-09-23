000100 01  MOD-W9O33501.                                                        
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
001200     03 MOD-IDTRANS          PIC X(4).                                    
001300*                                 BILDNUMMER                              
001400     03 MOD-IDMFSFEL         PIC X(3).                                    
001500*                                 MFS FELMEDDELANDE NUMMER                
001600     03 MOD-IDARTNR          PIC X(9).                                    
001700*                                 ARTIKELNUMMER                           
001800     03 MOD-IDORDNR7-NEXT    PIC 9(7).                                    
001900*                                 ORDERNUMMER                             
002000     03 MOD-IDDC-NEXT        PIC X(2).                                    
002100*                                 IDENTIFIERARE LAGER                     
002200     03 MOD-IDORDER-NEXT     PIC 9(7).                                    
002300*                                 VOLVO PARTS ORDERNUMMER                 
002400     03 MOD-ADLAGOMR-NEXT    PIC 9(2).                                    
002500*                                 LAGEROMRÅDE                             
002600     03 MOD-ADGANG-NEXT      PIC 9(2).                                    
002700*                                 GÅNG                                    
002800     03 MOD-ADPLATS-NEXT     PIC 9(5).                                    
002900*                                 LAGERPLATSNUMMER                        
003000     03 MOD-IDPRODNR-NEXT    PIC 9(7).                                    
003100*                                 PRODUKTIONSNUMMER                       
003200     03 MOD-IDKOLLI-NEXT     PIC 9(5).                                    
003300*                                 KOLLINUMMER                             
003400     03 MOD-IDRADNR-NEXT     PIC 9(4).                                    
003500*                                 RADNUMMER                               
003600     03 MOD-FLSVAR-NEXT      PIC X.                                       
003700*                                 ALLMÄN SVARSFLAGGA                      
003800     03 MOD-RAD              OCCURS 13 TIMES.                             
003900        05 MOD-IDORDNR7-RAD  PIC 9(7).                                    
004000*                                 ORDERNUMMER                             
004100        05 MOD-TIDISPIN      PIC 9(6).                                    
004200*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
004300        05 MOD-KVBEART-Q     PIC 9(7).                                    
004400*                                 BESTÄLLT KVANTANPASSAT ANTAL            
004500        05 MOD-KVPREAVB      PIC 9(7).                                    
004600*                                 PREL-AVB KVANT                          
004700        05 MOD-IDDC-RAD      PIC X(2).                                    
004800*                                 IDENTIFIERARE LAGER                     
004900        05 MOD-KDORDSTA      PIC X(2).                                    
005000*                                 VOLVOORDERSTATUS                        
005100        05 MOD-IDORDNR7-LEV  PIC 9(7).                                    
005200*                                 ORDERNUMMER                             
005300        05 MOD-TIANNULL      PIC 9(6).                                    
005400*                                 ANNULLATIONSDATUM (ÅÅMMDD)              
005500        05 MOD-KDORDKL       PIC 9.                                       
005600*                                 ORDERKLASS                              
005700        05 MOD-IDBIL.                                                     
005800*                                 BILIDENTITET                            
005900           07 MOD-IDBILTYP   PIC X(3).                                    
006000*                                 BILTYP                                  
006100           07 MOD-TIAAAA     PIC X(4).                                    
006200*                                 ÅRTAL (ÅÅÅÅ)                            
006300           07 MOD-IDCHASSI-PIE                                            
006400                             PIC X(6).                                    
006500*                                 CHASSINUMMER PIE                        
006600*** END OF VILMAII-COPY LENGTH= 812 BYTES                                 
