000100 01  MOD-W3O18401.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 3184              
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDFAKT-IN        PIC X(7).                                    
000800*                                 FAKTURANUMMER                           
000900     03 MOD-IDKOLLI-IN       PIC X(5).                                    
001000*                                 KOLLINUMMER                             
001100     03 MOD-FLINLI-IN        PIC X.                                       
001200*                                 INLAGD RAD, PARTI ELLER KOLLI           
001300     03 MOD-IDDC-REC-IN      PIC X(2).                                    
001400*                                 MOTTAGANDE LAGER                        
001500     03 MOD-KDPRT-ATTR       PIC X(2).                                    
001600*                                 MFS ATTRIBUTFÄLT                        
001700     03 MOD-KDPRT            PIC X(3).                                    
001800*                                 PRINTERKOD                              
001900     03 MOD-IDFAKT-UT        PIC X(7).                                    
002000*                                 FAKTURANUMMER                           
002100     03 MOD-IDKOLLI-UT       PIC X(5).                                    
002200*                                 KOLLINUMMER                             
002300     03 MOD-FLINLI-UT        PIC X.                                       
002400*                                 INLAGD RAD, PARTI ELLER KOLLI           
002500     03 MOD-IDDC-REC-UT      PIC X(2).                                    
002600*                                 MOTTAGANDE LAGER                        
002700     03 MOD-FAKTURA-RAD      OCCURS 11 TIMES.                             
002800*                                 ARTIKLAR I K-FAKTURA                    
002900*                                 CLEARING FLEN MAASTRICHT                
003000        05 MOD-KDCMD-ATTR    PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200        05 MOD-KDCMD         PIC X.                                       
003300*                                 RAD-UPPDATERINGSKOMMANDO                
003400*                                  BLANK  = INGENTING                     
003500*                                  D , B  = DELETE                        
003600*                                  R , Ä  = REPLACE                       
003700*                                  I,N,A  = INSERT                        
003800*                                  S , V  = SELECT                        
003900*                                  P , P  = PRINT                         
004000*                                  C , K  = COPY                          
004100        05 MOD-IDKOLLI       PIC X(5).                                    
004200*                                 KOLLINUMMER                             
004300        05 MOD-IDARTNR-OBJ   PIC Z(7)9.                                   
004400*                                 OBJEKTNUMMER                            
004500        05 MOD-BEART         PIC X(25).                                   
004600*                                 ARTIKELBENÄMNING                        
004700        05 MOD-KVLEVART      PIC Z(6)9.                                   
004800*                                 LEVERERAT ANTAL STYCK                   
004900        05 MOD-KVANTMOT-ATTR PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100        05 MOD-KVANTMOT      PIC Z(6)9.                                   
005200*                                 ANTAL MOTTAGET                          
005300     03 MOD-IDKOLLI-NY-ATTR  PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500     03 MOD-IDKOLLI-NY       PIC X(5).                                    
005600*                                 KOLLINUMMER                             
005700     03 MOD-IDARTNR-OBJ-NY-ATTR                                           
005800                             PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000     03 MOD-IDARTNR-OBJ-NY   PIC X(8).                                    
006100*                                 OBJEKTNUMMER                            
006200     03 MOD-KVANTMOT-NY-ATTR PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400     03 MOD-KVANTMOT-NY      PIC Z(6)9.                                   
006500*                                 ANTAL MOTTAGET                          
006600     03 MOD-IDKOLLI-KLAR-ATTR                                             
006700                             PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900     03 MOD-IDKOLLI-KLAR     PIC Z(4)9.                                   
007000*                                 KOLLINUMMER                             
007100     03 MOD-TEMFSINF         PIC X(55).                                   
007200*                                 INFORMATIONSMEDDELANDE                  
007300*** END OF VILMAII-COPY LENGTH= 794 BYTES                                 
