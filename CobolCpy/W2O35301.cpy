000100 01  MOD-W2O35301.                                                        
000200*                                 MOD-COPYTEXT FÖR W2035300               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-IDDC-IN          PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 MOD-IDDC-UT          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-BEART            PIC X(25).                                   
001600*                                 ARTIKELBENÄMNING                        
001700     03 MOD-GRP-RAD          OCCURS 13 TIMES.                             
001800        05 MOD-IDDC          PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000        05 MOD-FILLER        PIC X(2).                                    
002100        05 MOD-IDLEVNR       PIC X(5).                                    
002200*                                 LEVERANTÖRNUMMER                        
002300        05 MOD-FILLER        PIC X(4).                                    
002400        05 MOD-TIREGDAT      PIC 9(6).                                    
002500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002600        05 MOD-FILLER        PIC X.                                       
002700        05 MOD-KDORDKL       PIC 9.                                       
002800*                                 ORDERKLASS                              
002900        05 MOD-FILLER        PIC X.                                       
003000        05 MOD-IDKUNDRF      PIC X(10).                                   
003100*                                 KUNDENS REFERENS (ORDERID)              
003200        05 MOD-FILLER        PIC X.                                       
003300        05 MOD-KVBEART       PIC Z(7).                                    
003400*                                 BESTÄLLT ANTAL STYCKEN                  
003500        05 MOD-FILLER        PIC X.                                       
003600        05 MOD-KVRO          PIC Z(7).                                    
003700*                                 ANTAL RESTNOTERADE ARTIKLAR             
003800        05 MOD-FILLER        PIC X.                                       
003900        05 MOD-KVAVIS        PIC Z(7).                                    
004000*                                 AVISERAT ANTAL                          
004100        05 MOD-FILLER        PIC X.                                       
004200        05 MOD-IDFAKT        PIC Z(7).                                    
004300*                                 FAKTURANUMMER                           
004400        05 MOD-FILLER        PIC X.                                       
004500        05 MOD-TIIDFAKT      PIC 9(6).                                    
004600*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004700        05 MOD-FILLER        PIC X.                                       
004800        05 MOD-TIBERANK      PIC 9(6).                                    
004900*                                 BERÄKNAD ANKOMSTDATUM                   
005000     03 MOD-TEMFSINF         PIC X(55).                                   
005100*                                 INFORMATIONSMEDDELANDE                  
005200*** END OF VILMAII-COPY LENGTH= 1160 BYTES                                
