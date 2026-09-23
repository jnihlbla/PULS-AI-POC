000100 01  W61179.                                                              
000200*                                 INLEVERANSTRANSAKTIONSINFO              
000300*                                 FYSISK NYCKEL IDLOPNRM                  
000400     03 IDLOPNRM             PIC S9(9)           COMP-3.                  
000500*                                 L÷PNUMMER MOTTAGNINGSRAPPORT            
000600*                                 (0VVDLLLLK)                             
000700*                                 SERIAL NO RECEIVING REPORT              
000800*                                 (0WWDLLLLC)                             
000900     03 TIAAVVD              PIC S9(5)           COMP-3.                  
001000*                                 ≈R - VECKA - DAG   (≈≈VVD)              
001100*                                 YEAR - WEEK - DAY  (YYWWD)              
001200     03 TIPP                 PIC S9(2)           COMP-3.                  
001300*                                 PLANERINGSPERIOD (PP)                   
001400*                                 12 PER ≈R                               
001500*                                 PLANNING PERIOD  (PP)                   
001600*                                 12 PER YEAR                             
001700     03 W611R31.                                                          
001800        05 IDPTYP            PIC X(3).                                    
001900*                                 POSTTYP                                 
002000*                                 RECORD TYPE                             
002100        05 IDDC              PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300*                                 WAREHOUSE IDENTIFIER                    
002400        05 IDARTNR           PIC S9(9)           COMP-3.                  
002500*                                 ARTIKELNUMMER                           
002600*                                 PART NUMBER                             
002700        05 IDPLFORM          PIC S9(3)           COMP-3.                  
002800*                                 PLATTFORMSNUMMER                        
002900*                                 PLATFORM NUMBER                         
003000        05 IDLEVNR-INL       PIC X(5).                                    
003100*                                 LEVERANT÷R F÷R AKTUELL INLEV.           
003200        05 KDRT              PIC S9(3)           COMP-3.                  
003300*                                 REDOVISNINGSTYP                         
003400*                                 TYPE OF ACCOUNTING                      
003500        05 TIAVSDAT          PIC S9(7)           COMP-3.                  
003600*                                 AVISERINGSDATUM (YYMMDD)                
003700*                                 ADVICE NOTE DATE                        
003800        05 IDKONTO           PIC S9(11)          COMP-3.                  
003900*                                 KONTO                                   
004000*                                 ACCOUNT                                 
004100        05 IDAVINR           PIC S9(7)           COMP-3.                  
004200*                                 AVI-NUMMER                              
004300*                                 ADVICE NOTE NUMBER                      
004400        05 KVAVIS            PIC S9(7)           COMP-3.                  
004500*                                 AVISERAT ANTAL                          
004600*                                 QUANTITY NOTIFIED                       
004700        05 FILLER            PIC X(4).                                    
004800        05 KDAVVANT          PIC S9              COMP-3.                  
004900*                                 AVVIKELSEANTAL KOD                      
005000*                                 0=INGEN ANM.   1=AVVIKELSE              
005100*                                 2=MAKULERING AV MOTT.RAPPORT            
005200*                                 QUANTITY DEVIATION  CODE                
005300*                                 0=NO DEV.    1=DEVIATION                
005400*                                 2=CANCELLING OF REC. REPORT             
005500        05 KVANTMOT          PIC S9(7)           COMP-3.                  
005600*                                 ANTAL MOTTAGET                          
005700*                                 QUANTITY RECEIVED                       
005800        05 KVFORDEL          PIC S9(7)           COMP-3.                  
005900*                                 ANTAL F÷RDELAT                          
006000*                                 SPLIT QUANTITY                          
006100        05 IDKOLLI           PIC S9(7)           COMP-3.                  
006200*                                 KOLLINUMMER         IDKOLLI-002         
006300        05 KDAVVKV           PIC S9              COMP-3.                  
006400*                                 KVALITETSAVVIKELSEKOD                   
006500*                                 0=INGEN ANM.  1=AVVIKELSE               
006600*                                 2=AVVIKELSE, RETURNERAS                 
006700*                                 QUALITY DEVIATION CODE                  
006800*                                 0 = NO DEV.  1 = DEVIATION              
006900*                                 2 = DEVIATION, WILL BE RETURNED         
007000        05 KVRETUR           PIC S9(7)           COMP-3.                  
007100*                                 ANTAL I RETUR                           
007200*                                 QUANTITY IN RETURN                      
007300        05 FILLER            PIC X(2).                                    
007400        05 ADLAGOMR          PIC S9(3)           COMP-3.                  
007500*                                 LAGEROMR≈DE                             
007600*                                 AREA                                    
007700        05 BEFT              PIC S9(3)           COMP-3.                  
007800*                                 F÷RPACKNINGSTYP                         
007900*                                 PACKAGING TYPE                          
008000        05 PRARTSTD          PIC S9(7)V9(2)      COMP-3.                  
008100*                                 ARTIKELSTANDARDPRIS                     
008200*                                 STANDARD PRICE                          
008300        05 KDFORP            PIC S9(5)           COMP-3.                  
008400*                                 F÷RPACKNINGSKOD                         
008500*                                 PACKAGING CODE                          
008600        05 KVPB-SEP          PIC S9(6)V9(1)      COMP-3.                  
008700*                                 SEPARAT PERIODBEHOV                     
008800*                                 SEPARATE PERIOD REQUIREMENTS            
008900        05 KVPB-SATS         PIC S9(6)V9(1)      COMP-3.                  
009000*                                 SATS-PERIODBEHOV                        
009100*                                 KIT PERIOD REQUIREMENTS                 
009200        05 KDEMBKOD-0        PIC S9(3)           COMP-3.                  
009300*                                 EMBALLAGEKOD 0                          
009400        05 KDEMBKOD-1        PIC S9(3)           COMP-3.                  
009500*                                 EMBALLAGEKOD 1                          
009600        05 KDEMBKOD-2        PIC S9(3)           COMP-3.                  
009700*                                 EMBALLAGEKOD 2                          
009800        05 KVQPACK-0         PIC S9(5)           COMP-3.                  
009900*                                 ANTAL I Q0 F÷RPACKNING                  
010000        05 KVQPACK-1         PIC S9(5)           COMP-3.                  
010100*                                 ANTAL I Q1 F÷RPACKNING                  
010200*                                 QUANTITY IN BULK PACK Q1                
010300        05 KVQPACK-2         PIC S9(5)           COMP-3.                  
010400*                                 ANTAL I Q2 F÷RPACKNING                  
010500*                                 QUANTITY IN BULK PACK Q2                
010600*** END OF VILMAII-COPY LENGTH= 106 BYTES                                 
