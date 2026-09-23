000100 01  MOD-W6O11901.                                                        
000200*                                 MODCOPYTEXT TILL W60119.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500*                                 SCREEN NUMBER                           
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800*                                 MFS ERROR MESSAGE                       
000900     03 MOD-IDLOPNRM-IN      PIC X(8).                                    
001000*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001100*                                 (0VVDLLLLK)                             
001200*                                 SERIAL NO RECEIVING REPORT              
001300*                                 (0WWDLLLLC)                             
001400     03 MOD-IDDC-IN          PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600*                                 WAREHOUSE IDENTIFIER                    
001700     03 MOD-IDLOPNRM-UT      PIC X(8).                                    
001800*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001900*                                 (0VVDLLLLK)                             
002000*                                 SERIAL NO RECEIVING REPORT              
002100*                                 (0WWDLLLLC)                             
002200     03 MOD-IDDC-UT          PIC X(2).                                    
002300*                                 IDENTIFIERARE LAGER                     
002400*                                 WAREHOUSE IDENTIFIER                    
002500     03 MOD-IDARTNR          PIC Z(7)9.                                   
002600*                                 ARTIKELNUMMER                           
002700*                                 PART NUMBER                             
002800     03 MOD-KVAVIS           PIC Z(5)9.                                   
002900*                                 AVISERAT ANTAL                          
003000*                                 QUANTITY NOTIFIED                       
003100     03 MOD-BEART            PIC X(25).                                   
003200*                                 ARTIKELBENÄMNING                        
003300*                                 PART DESCRIPTION                        
003400     03 MOD-IDLEVNR          PIC X(5).                                    
003500*                                 LEVERANTÖRNUMMER                        
003600*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
003700     03 MOD-IDFS             PIC X(8).                                    
003800*                                 FÖLJESEDELSNUMMER ENL ODETTE            
003900*                                 ADVICE NOTE NUMBER ODETTE               
004000     03 MOD-TIAVIDAT         PIC 9(6).                                    
004100*                                 AVISERINGSDATUM (YYMMDD)                
004200*                                 ADVICE NOTE DATE                        
004300     03 MOD-FLMAK-ATTR       PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500     03 MOD-FLMAK            PIC X.                                       
004600*                                 ALLMÄN FLAGGA                           
004700*                                 GENERAL FLAG                            
004800     03 MOD-FLBACK-ATTR      PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000     03 MOD-FLBACK           PIC X.                                       
005100*                                 ALLMÄN FLAGGA                           
005200*                                 GENERAL FLAG                            
005300     03 MOD-TEMFSINF         PIC X(55).                                   
005400*                                 INFORMATIONSMEDDELANDE                  
005500*                                 INFORMATION MESSAGE                     
005600*** END OF VILMAII-COPY LENGTH= 183 BYTES                                 
