000100 01  MOD-W5O16201.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 5162              
000300*                                 PRIME COUNT SELECTION                   
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
003200     03 MOD-IDTRANS-IN       PIC X(4).                                    
003300*                                 BILDNUMMER                              
003400     03 MOD-IDTRANS-UT       PIC X(4).                                    
003500*                                 BILDNUMMER                              
003600     03 MOD-KVLSTOTAL        PIC Z(7).                                    
003700*                                 ANTAL                                   
003800     03 MOD-KVCHUP           PIC Z(5)9-.                                  
003900*                                 ANTAL                                   
004000     03 MOD-KVCHDO           PIC Z(5)9-.                                  
004100*                                 ANTAL                                   
004200     03 MOD-FLEXTRAKT-ATTR   PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400     03 MOD-FLEXTRAKT        PIC X.                                       
004500*                                 ALLMÄN FLAGGA                           
004600     03 MOD-KVTOTAL          PIC Z(7).                                    
004700*                                 ANTAL                                   
004800     03 MOD-TABELLRAD        OCCURS 11 TIMES.                             
004900*                                 GRUPP MED TABELL RADER                  
005000        05 MOD-SELECT-RAD-ATTR                                            
005100                             PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300        05 MOD-SELECT-RAD    PIC X.                                       
005400        05 MOD-IDDC          PIC X(2).                                    
005500*                                 IDENTIFIERARE LAGER                     
005600        05 MOD-IDHUVTYP      PIC X(4).                                    
005700*                                 LOGGTYP EKONOMISK HÄNDELSE              
005800        05 MOD-IDSUBTYP      PIC X(3).                                    
005900*                                 LOGGTYP EKONOMISK HÄNDELSE              
006000        05 MOD-TIREGDAT      PIC 9(6).                                    
006100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
006200        05 MOD-KVART-SALDO   PIC Z(5)9-.                                  
006300*                                 ANTAL SALDOFÖRÄNDRADE ARTIKLAR          
006400        05 MOD-IDTECKEN-KVAKS-PAV                                         
006500                             PIC X.                                       
006600        05 MOD-KVAKS-PAV     PIC Z(6)9-.                                  
006700*                                 DEL AV AK PÅ VÄG                        
006800        05 MOD-IDTECKEN-KVAKS                                             
006900                             PIC X.                                       
007000        05 MOD-KVAKS         PIC Z(6)9-.                                  
007100*                                 ANKOMSTSALDO                            
007200        05 MOD-IDTECKEN-KVEFRS                                            
007300                             PIC X.                                       
007400        05 MOD-KVEFRS        PIC Z(6)9-.                                  
007500*                                 EJ FAKTURERAT ANTAL STYCK               
007600        05 MOD-IDTECKEN-KVLS PIC X.                                       
007700        05 MOD-KVLS          PIC Z(6)9-.                                  
007800*                                 LAGERSALDO                              
007900     03 MOD-TEMFSINF         PIC X(55).                                   
008000*                                 INFORMATIONSMEDDELANDE                  
008100*** END OF VILMAII-COPY LENGTH= 869 BYTES                                 
