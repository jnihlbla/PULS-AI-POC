000100 01  MOD-W6O10401.                                                        
000200*                                 MODCOPYTEXT TILL W60104.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDLEVNR-KOLLI-IN PIC X(5).                                    
000800*                                 LEVERANTÖRNUMMER KOLLI                  
000900     03 MOD-IDOKOLLI-IN      PIC X(9).                                    
001000*                                 ODETTE KOLLINUMMER                      
001100     03 MOD-IDLOPNRM-IN      PIC X(9).                                    
001200*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001300*                                 (0VVDLLLLK)                             
001400     03 MOD-IDDC-IN          PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 MOD-IDLEVNR-KOLLI-UT PIC X(5).                                    
001700*                                 LEVERANTÖRNUMMER KOLLI                  
001800     03 MOD-IDOKOLLI-UT      PIC X(9).                                    
001900*                                 ODETTE KOLLINUMMER                      
002000     03 MOD-IDLOPNRM-UT      PIC X(9).                                    
002100*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
002200*                                 (0VVDLLLLK)                             
002300     03 MOD-IDDC-UT          PIC X(2).                                    
002400*                                 IDENTIFIERARE LAGER                     
002500     03 MOD-IDRADNR-INL-ENTER                                             
002600                             PIC Z(7)9.                                   
002700*                                 ARTIKELNUMMER                           
002800     03 MOD-IDLEVNR-ENTER    PIC X(5).                                    
002900*                                 LEVERANTÖRNUMMER                        
003000     03 MOD-IDFS-ENTER       PIC X(8).                                    
003100*                                 FÖLJESEDELSNUMMER ENL ODETTE            
003200     03 MOD-TIAVIDAT-ENTER   PIC 9(6).                                    
003300*                                 AVISERINGSDATUM (YYMMDD)                
003400     03 MOD-IDRADNR-INL-NEXT PIC Z(7)9.                                   
003500*                                 ARTIKELNUMMER                           
003600     03 MOD-IDLEVNR-NEXT     PIC X(5).                                    
003700*                                 LEVERANTÖRNUMMER                        
003800     03 MOD-IDFS-NEXT        PIC X(8).                                    
003900*                                 FÖLJESEDELSNUMMER ENL ODETTE            
004000     03 MOD-TIAVIDAT-NEXT    PIC 9(6).                                    
004100*                                 AVISERINGSDATUM (YYMMDD)                
004200     03 MOD-IDLOPNRM-DEF     PIC 9(9).                                    
004300*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
004400*                                 (0VVDLLLLK)                             
004500     03 MOD-IDRADNR-DEF      PIC 9(4).                                    
004600*                                 RADNUMMER                               
004700     03 MOD-RADER.                                                        
004800*                                 RADER                                   
004900        05 MOD-IDARTNR-RAD   OCCURS 11 TIMES                              
005000                             PIC Z(7)9.                                   
005100*                                 ARTIKELNUMMER                           
005200        05 MOD-BEART-RAD     OCCURS 11 TIMES                              
005300                             PIC X(25).                                   
005400*                                 ARTIKELBENÄMNING                        
005500        05 MOD-INKLATTR      OCCURS 11 TIMES.                             
005600*                                 ATTR + FÄLT                             
005700           07 MOD-KVINLART-RAD-ATTR                                       
005800                             PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000           07 MOD-KVINLART-RAD                                            
006100                             PIC Z(5)9.                                   
006200*                                 ANTAL I PARTIRAD                        
006300        05 MOD-IDLOPNRM-RAD  OCCURS 11 TIMES                              
006400                             PIC Z(8)9.                                   
006500*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
006600*                                 (0VVDLLLLK)                             
006700        05 MOD-IDRADNR-RAD   OCCURS 11 TIMES                              
006800                             PIC Z(3)9.                                   
006900*                                 RADNUMMER                               
007000        05 MOD-BEFARLIG-RAD  OCCURS 11 TIMES                              
007100                             PIC X(10).                                   
007200     03 MOD-KVINLART-UPD-ATTR                                             
007300                             PIC X(2).                                    
007400*                                 MFS ATTRIBUTFÄLT                        
007500     03 MOD-KVINLART-UPD     PIC X(6).                                    
007600*                                 ANTAL I PARTIRAD                        
007700     03 MOD-IDLOPNRM-UPD-ATTR                                             
007800                             PIC X(2).                                    
007900*                                 MFS ATTRIBUTFÄLT                        
008000     03 MOD-IDLOPNRM-UPD     PIC X(9).                                    
008100*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
008200*                                 (0VVDLLLLK)                             
008300     03 MOD-IDRADNR-UPD-ATTR PIC X(2).                                    
008400*                                 MFS ATTRIBUTFÄLT                        
008500     03 MOD-IDRADNR-UPD      PIC X(4).                                    
008600*                                 RADNUMMER                               
008700     03 MOD-ADINLOMR-PRT-UPD-ATTR                                         
008800                             PIC X(2).                                    
008900*                                 MFS ATTRIBUTFÄLT                        
009000     03 MOD-ADINLOMR-PRT-UPD PIC X(4).                                    
009100*                                 INLEVERANSOMRÅDE                        
009200     03 MOD-TEMFSINF         PIC X(55).                                   
009300*                                 INFORMATIONSMEDDELANDE                  
009400*** END OF VILMAII-COPY LENGTH= 951 BYTES                                 
