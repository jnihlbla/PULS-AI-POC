000100 01  MOD-W6O18301.                                                        
000200*                                 MOD COPYTEXT FÖR W60183                 
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDLEVNR-IN       PIC X(5).                                    
000800*                                 LEVERANTÖRNUMMER                        
000900     03 MOD-IDLEVNR-UT       PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100     03 MOD-IDARTNR-IN       PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 MOD-IDARTNR-UT       PIC X(9).                                    
001400*                                 ARTIKELNUMMER                           
001500     03 MOD-IDFPINST-IN      PIC X(7).                                    
001600*                                 FÖRPACKNINGSINSTRUKTION NR              
001700     03 MOD-IDFPINST-UT      PIC X(7).                                    
001800*                                 FÖRPACKNINGSINSTRUKTION NR              
001900     03 MOD-IDFPINST-ATTR    PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100     03 MOD-IDFPINST         PIC Z(7).                                    
002200*                                 FÖRPACKNINGSINSTRUKTION NR              
002300     03 MOD-IDARTNR-ATTR     PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500     03 MOD-IDARTNR-KOP      PIC X(9).                                    
002600*                                 ARTIKELNUMMER                           
002700     03 MOD-IDLEVNR-ATTR     PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-IDLEVNR-KOP      PIC X(5).                                    
003000*                                 LEVERANTÖRNUMMER                        
003100     03 MOD-TEXTRAD          PIC X(16).                                   
003200     03 MOD-BEFT             PIC Z9.                                      
003300*                                 FÖRPACKNINGSTYP                         
003400     03 MOD-TIREGDAT         PIC 9(6).                                    
003500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003600     03 MOD-TIUPPDAT         PIC 9(6).                                    
003700*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
003800     03 MOD-FLNY-ATTR        PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000     03 MOD-FLNY             PIC X.                                       
004100*                                 ALLMÄN SVARSFLAGGA                      
004200     03 MOD-IDPERSON-ATTR    PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400     03 MOD-IDPERSON         PIC Z(2)9.                                   
004500*                                 PERSONKOD                               
004600     03 MOD-FLBORT-ATTR      PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800     03 MOD-FLBORT           PIC X.                                       
004900*                                 ALLMÄN SVARSFLAGGA                      
005000     03 MOD-RAD              OCCURS 10 TIMES.                             
005100        05 MOD-TEFPINST-ATTR PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300        05 MOD-TEFPINST      PIC X(60).                                   
005400     03 MOD-IDLTERM-ATTR     PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600     03 MOD-IDLTERM          PIC X(8).                                    
005700*                                 LOGISKT TERMINALNAMN                    
005800     03 MOD-TEMFSINF         PIC X(55).                                   
005900*                                 INFORMATIONSMEDDELANDE                  
006000*** END OF VILMAII-COPY LENGTH= 839 BYTES                                 
