000100 01  MOD-W6O16501.                                                        
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
001600     03 MOD-IDINLEV-NEXT     PIC 9(15).                                   
001700*                                 INLEVERANS NUMMER                       
001800     03 MOD-TIREGDAT-NEXT    PIC 9(6).                                    
001900*                                 REGISTRERINGSDATUM (≈≈MMDD)             
002000     03 MOD-INFO-RAD         OCCURS 14 TIMES.                             
002100*                                 RADINFORMATION                          
002200        05 MOD-IDPTYP        PIC X(3).                                    
002300*                                 POSTTYP                                 
002400        05 FILLER            PIC X.                                       
002500        05 MOD-IDLOPNRM      PIC Z(7)9.                                   
002600*                                 L÷PNUMMER MOTTAGNINGSRAPPORT            
002700*                                 (0VVDLLLLK)                             
002800        05 FILLER            PIC X.                                       
002900        05 MOD-TIAAVVD       PIC 9(5).                                    
003000*                                 ≈R - VECKA - DAG   (≈≈VVD)              
003100        05 FILLER            PIC X.                                       
003200        05 MOD-IDLEVNR       PIC X(5).                                    
003300*                                 LEVERANT÷RNUMMER                        
003400        05 FILLER            PIC X.                                       
003500        05 MOD-KDRT          PIC Z9.                                      
003600*                                 REDOVISNINGSTYP                         
003700        05 FILLER            PIC X.                                       
003800        05 MOD-ADLAGOMR      PIC Z9.                                      
003900*                                 LAGEROMR≈DE                             
004000        05 FILLER            PIC X.                                       
004100        05 MOD-ADGANG        PIC Z9.                                      
004200*                                 G≈NG                                    
004300        05 FILLER            PIC X.                                       
004400        05 MOD-ADPLATS       PIC Z(4)9.                                   
004500*                                 LAGERPLATSNUMMER                        
004600        05 MOD-FILLERX2      PIC X(2).                                    
004700        05 MOD-KDAKSTAT      PIC 9.                                       
004800*                                 INLEVERANS STATUS                       
004900        05 MOD-FILLERX2      PIC X(2).                                    
005000        05 MOD-IDFS          PIC X(8).                                    
005100*                                 F÷LJESEDELSNUMMER ENL ODETTE            
005200        05 MOD-IDAVINR REDEFINES MOD-IDFS                                 
005300                             PIC Z(7)9.                                   
005400*                                 AVINUMMER           IDAVINR-002         
005500        05 FILLER            PIC X.                                       
005600        05 MOD-KVAVIS        PIC -(6)9.                                   
005700*                                 AVISERAT ANTAL                          
005800        05 FILLER            PIC X.                                       
005900        05 MOD-KVFORDEL      PIC Z(5)9.                                   
006000*                                 ANTAL F÷RDELAT                          
006100        05 FILLER            PIC X.                                       
006200        05 MOD-KVRETUR       PIC -(6)9.                                   
006300*                                 ANTAL I RETUR                           
006400        05 MOD-FILLERX2      PIC X(2).                                    
006500        05 MOD-KDAVVANT      PIC 9.                                       
006600*                                 AVVIKELSEANTAL KOD                      
006700*                                 0=INGEN ANM.   1=AVVIKELSE              
006800*                                 2=MAKULERING AV MOTT.RAPPORT            
006900     03 MOD-TEMFSINF         PIC X(55).                                   
007000*                                 INFORMATIONSMEDDELANDE                  
007100*** END OF VILMAII-COPY LENGTH= 1234 BYTES                                
