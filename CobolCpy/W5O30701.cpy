000100 01  MOD-W5O30701.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 5307              
000300*                                 INVENTERING                             
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDARTNR-IN       PIC X(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 MOD-IDARTNR-UT       PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200     03 MOD-IDDC-IN          PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 MOD-IDDC-UT          PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 MOD-IDDC-ENTER       PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800     03 MOD-IDDC-NEXT        PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000     03 MOD-IDINLEV-ENTER    PIC 9(15).                                   
002100*                                 INLEVERANS NUMMER                       
002200     03 MOD-IDINLEV-NEXT     PIC 9(15).                                   
002300*                                 INLEVERANS NUMMER                       
002400     03 MOD-BEART            PIC X(25).                                   
002500*                                 ARTIKELBENÄMNING                        
002600     03 MOD-SDC-LAGER        PIC X(8).                                    
002700     03 MOD-RAD              OCCURS 13 TIMES.                             
002800        05 MOD-IDPTYP        PIC X(3).                                    
002900*                                 POSTTYP                                 
003000        05 MOD-IDDC          PIC X(2).                                    
003100*                                 IDENTIFIERARE LAGER                     
003200        05 MOD-TIREGDAT      PIC 9(6).                                    
003300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003400        05 MOD-TIINLMOT      PIC 9(6).                                    
003500*                                 MOTTAGNINGSDATUM   (ÅÅMMDD)             
003600        05 MOD-TIINLINL      PIC 9(6).                                    
003700*                                 RAPPORTERINGSDATUM INLAGD (R32)         
003800        05 MOD-IDKUNDRF      PIC X(10).                                   
003900*                                 KUNDENS REFERENS (ORDERID)              
004000        05 MOD-IDFAKT        PIC Z(6)9.                                   
004100*                                 FAKTURANUMMER                           
004200        05 MOD-KVAVIS        PIC Z(5)9.                                   
004300*                                 AVISERAT ANTAL                          
004400        05 MOD-KVANTMOT      PIC -(6)9.                                   
004500*                                 ANTAL MOTTAGET                          
004600        05 MOD-ADLAGOMR      PIC Z9.                                      
004700*                                 LAGEROMRÅDE                             
004800        05 MOD-ADGANG        PIC Z9.                                      
004900*                                 GÅNG                                    
005000        05 MOD-ADPLATS       PIC Z(4)9.                                   
005100*                                 LAGERPLATSNUMMER                        
005200     03 MOD-TEMFSINF         PIC X(55).                                   
005300*                                 INFORMATIONSMEDDELANDE                  
005400*** END OF VILMAII-COPY LENGTH= 994 BYTES                                 
