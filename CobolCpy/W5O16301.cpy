000100 01  MOD-W5O16301.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 5163              
000300*                                 BALANCE LOG OVERVIEW - 2                
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDARTNR-IN       PIC Z(8)9.                                   
000900*                                 ARTIKELNUMMER                           
001000     03 MOD-IDARTNR-UT       PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200     03 MOD-IDDC-IN          PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 MOD-IDDC-UT          PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 MOD-IDHUVTYP-IN      PIC X(4).                                    
001700*                                 LOGGTYP EKONOMISK HÄNDELSE              
001800     03 MOD-IDHUVTYP-UT      PIC X(4).                                    
001900*                                 LOGGTYP EKONOMISK HÄNDELSE              
002000     03 MOD-IDSUBTYP-IN      PIC X(3).                                    
002100*                                 LOGGTYP EKONOMISK HÄNDELSE              
002200     03 MOD-IDSUBTYP-UT      PIC X(3).                                    
002300*                                 LOGGTYP EKONOMISK HÄNDELSE              
002400     03 MOD-TIREGDAT-IN1     PIC 9(6).                                    
002500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002600     03 MOD-TIREGDAT-UT1     PIC 9(6).                                    
002700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002800     03 MOD-TIREGDAT-IN2     PIC 9(6).                                    
002900*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003000     03 MOD-TIREGDAT-UT2     PIC 9(6).                                    
003100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003200     03 MOD-IDSALDO-IN       PIC X(4).                                    
003300*                                 BILDNUMMER                              
003400     03 MOD-IDSALDO-UT       PIC X(4).                                    
003500*                                 BILDNUMMER                              
003600     03 MOD-KVTOTAL          PIC Z(7).                                    
003700*                                 ANTAL                                   
003800     03 MOD-KVCHUP           PIC Z(5)9-.                                  
003900*                                 ANTAL                                   
004000     03 MOD-KVCHDO           PIC Z(5)9-.                                  
004100*                                 ANTAL                                   
004200     03 MOD-FLEXTRAKT-ATTR   PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400     03 MOD-FLEXTRAKT        PIC X.                                       
004500*                                 ALLMÄN FLAGGA                           
004600     03 MOD-TABELLRAD        OCCURS 11 TIMES.                             
004700*                                 GRUPP MED TABELL RADER                  
004800        05 MOD-SELECT-RAD-ATTR                                            
004900                             PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100        05 MOD-SELECT-RAD    PIC X.                                       
005200        05 MOD-IDDC          PIC X(2).                                    
005300*                                 IDENTIFIERARE LAGER                     
005400        05 MOD-IDHUVTYP      PIC X(4).                                    
005500*                                 LOGGTYP EKONOMISK HÄNDELSE              
005600        05 MOD-IDSUBTYP      PIC X(3).                                    
005700*                                 LOGGTYP EKONOMISK HÄNDELSE              
005800        05 MOD-TIREGDAT      PIC 9(6).                                    
005900*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
006000        05 MOD-KVART-SALDO   PIC Z(5)9-.                                  
006100*                                 ANTAL SALDOFÖRÄNDRADE ARTIKLAR          
006200        05 MOD-IDTECKEN      PIC X.                                       
006300        05 MOD-KVSALDO       PIC Z(6)9-.                                  
006400*                                 ANTAL                                   
006500     03 MOD-TEMFSINF         PIC X(55).                                   
006600*                                 INFORMATIONSMEDDELANDE                  
006700*** END OF VILMAII-COPY LENGTH= 565 BYTES                                 
