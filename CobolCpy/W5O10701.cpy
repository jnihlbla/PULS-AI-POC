000100 01  MOD-W5O10701.                                                        
000200*                                 MOD-COPYTEXT F÷R BILD                   
000300*                                 INLEVERANSINFORMATION                   
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
001600     03 MOD-DATUM-IN         PIC X(6).                                    
001700*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
001800     03 MOD-DATUM-UT         PIC X(6).                                    
001900*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
002000     03 MOD-IDFS-IN          PIC X(8).                                    
002100*                                 F÷LJESEDELSNUMMER ENL ODETTE            
002200     03 MOD-IDFS-UT          PIC X(8).                                    
002300*                                 F÷LJESEDELSNUMMER ENL ODETTE            
002400     03 MOD-IDPTYP-IN        PIC X(3).                                    
002500*                                 POSTTYP                                 
002600     03 MOD-IDPTYP-UT        PIC X(3).                                    
002700*                                 POSTTYP                                 
002800     03 MOD-IDLEVNR-IN       PIC X(5).                                    
002900*                                 LEVERANT÷RNUMMER                        
003000     03 MOD-IDLEVNR-UT       PIC X(5).                                    
003100*                                 LEVERANT÷RNUMMER                        
003200     03 MOD-KDRT-IN          PIC X(2).                                    
003300*                                 REDOVISNINGSTYP                         
003400     03 MOD-KDRT-UT          PIC Z9.                                      
003500*                                 REDOVISNINGSTYP                         
003600     03 MOD-IDINLEV-NEXT     PIC 9(15).                                   
003700*                                 INLEVERANS NUMMER                       
003800     03 MOD-INFO-RAD         OCCURS 14 TIMES.                             
003900*                                 RADINFORMATION                          
004000        05 MOD-CMD-ATTR      PIC X(2).                                    
004100*                                 MFS ATTRIBUTFƒLT                        
004200        05 MOD-CMD           PIC X.                                       
004300        05 MOD-IDPTYP        PIC X(3).                                    
004400*                                 POSTTYP                                 
004500        05 MOD-IDLOPNRM      PIC Z(7)9.                                   
004600*                                 L÷PNUMMER MOTTAGNINGSRAPPORT            
004700*                                 (0VVDLLLLK)                             
004800        05 MOD-TIAAVVD       PIC 9(5).                                    
004900*                                 ≈R - VECKA - DAG   (≈≈VVD)              
005000        05 MOD-IDLEVNR       PIC X(5).                                    
005100*                                 LEVERANT÷RNUMMER                        
005200        05 MOD-KDRT          PIC Z9.                                      
005300*                                 REDOVISNINGSTYP                         
005400        05 MOD-TIAVSDAT      PIC 9(6).                                    
005500*                                 AVISERINGSDATUM (YYMMDD)                
005600        05 MOD-IDFS          PIC X(8).                                    
005700*                                 F÷LJESEDELSNUMMER ENL ODETTE            
005800        05 MOD-IDAVINR REDEFINES MOD-IDFS                                 
005900                             PIC Z(7)9.                                   
006000*                                 AVINUMMER           IDAVINR-002         
006100        05 MOD-KVAVIS        PIC -(6)9.                                   
006200*                                 AVISERAT ANTAL                          
006300        05 MOD-KVANTMOT      PIC -(6)9.                                   
006400*                                 ANTAL MOTTAGET                          
006500        05 MOD-KVRETUR       PIC -(6)9.                                   
006600*                                 ANTAL I RETUR                           
006700        05 MOD-KDAVVANT      PIC 9.                                       
006800*                                 AVVIKELSEANTAL KOD                      
006900*                                 0=INGEN ANM.   1=AVVIKELSE              
007000*                                 2=MAKULERING AV MOTT.RAPPORT            
007100     03 MOD-TEMFSINF         PIC X(55).                                   
007200*                                 INFORMATIONSMEDDELANDE                  
007300*** END OF VILMAII-COPY LENGTH= 1052 BYTES                                
