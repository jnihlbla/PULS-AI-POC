000100*COMPOPT VPOSIX=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W6016400.                                                
000400 AUTHOR.         UMESH JAIN.                                              
000500 DATE-WRITTEN.   08/11/27.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNCTION:                                                            
000900*        MPP FOR REPLENISHMENT OF PROPOSALS                               
001000*                                                                         
001100*        THE PROGRAM UPDATES   WDT1                                       
001200*        THE PROGRAM READS     WDT1A                                      
001300*                              WDT1B                                      
001400*                              WDT1C                                      
001500*                              WDT1D                                      
001600*                              WDT1E                                      
001700*                              WDT1F                                      
001800*                              WDT1G                                      
001900*                              WDT1H                                      
002000*                              WDT1I                                      
002100*        THE PROGRAM UPDATES   WDK6                                       
002200*        THE PROGRAM UPDATES   WDD8                                       
002300*        THE PROGRAM UPDATES   WDJ9                                       
002400*                                                                         
002500*    INDATA.                                                              
002600*        TRANSACTION: W6T164                                              
002700*        MID:         W6I16401                                            
002800*                                                                         
002900*    OUTDATA.                                                             
003000*        MOD:         W6O16401                                            
003100                                                                          
003200*    JUMP TRANSACTION W6T162                                              
003300                                                                          
003400     SKIP3                                                                
003500 ENVIRONMENT DIVISION.                                                    
003600                                                                          
003700 DATA DIVISION.                                                           
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000 77  IDPGM                       PIC X(08)   VALUE 'W6016400'.            
004100                                                                          
004200*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004300 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004400                                                                          
004500 77  YES                         PIC X       VALUE 'Y'.                   
004600 77  NOO                         PIC X       VALUE 'N'.                   
004700                                                                          
004800*    --- INDEX FOR SCROLL LINES                                           
004900 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005000 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
005100 77  W-INDX                      PIC S9(4)  VALUE +0    COMP SYNC.        
005200 77  W-CMD-X                     PIC 9(2)   VALUE ZERO.                   
005300 77  W-CMD-C                     PIC 9(2)   VALUE ZERO.                   
005400*    --- WORKFIELDS FOR KEYS                                              
005500 77  W-ADLAGOMR-FOM              PIC X(2)   VALUE  SPACES.                
005600 77  W-ADLAGOMR-TOM              PIC X(2)   VALUE  SPACES.                
005700 77  W-ADGANG-FOM                PIC X(2)   VALUE  SPACES.                
005800 77  W-ADGANG-TOM                PIC X(2)   VALUE  SPACES.                
005900 77  W-KDSTAPF                   PIC X(1)   VALUE  SPACES.                
006000 77  W-KDPRIO-PF                 PIC X(1)   VALUE  SPACES.                
006100 77  W-IDUSER                    PIC X(7)   VALUE  SPACES.                
006200 77  WS-IDARTNR                  PIC X(9)   VALUE  SPACES.                
006300 77  W-KVBEST-ANDR               PIC 9(6)   VALUE  ZERO.                  
006400 77  W-KVBEST-ANDR-NEW           PIC X(6)   VALUE  SPACES.                
006500 77  W-KVBEST-ANDR-NEW-N         PIC 9(6)   VALUE  ZERO.                  
006600 77  W-ADLAGOMR-FOM-NEW          PIC X(2)   VALUE  SPACES.                
006700 77  W-ADLAGOMR-FOM-NEW-N        PIC 9(2)   VALUE  ZERO.                  
006800 77  W-ADGANG-FOM-NEW            PIC X(2)   VALUE  SPACES.                
006900 77  W-ADGANG-FOM-NEW-N          PIC 9(2)   VALUE  ZERO.                  
007000 77  W-ADPLATS-FOM-NEW           PIC X(5)   VALUE  SPACES.                
007100 77  W-ADPLATS-FOM-NEW-N         PIC 9(5)   VALUE  ZERO.                  
007200 77  W-TALLY                     PIC 9(2)   VALUE  ZERO.                  
007300 77  W-LEN                       PIC 9(2)   VALUE  ZERO.                  
007400 01  W-ORDTIME.                                                           
007500     03 W-DDDDDD PIC X(6).                                                
007600     03 W-TTTTTT PIC X(6).                                                
007700*    --- WORK FIELDS FOR ACTUAL                                           
007800                                                                          
007900 77  UPDATE-SW                   PIC X       VALUE 'N'.                   
008000     88  UPDATE-SUCCESS                      VALUE 'Y'.                   
008100     88  NO-UPDATE                           VALUE 'N'.                   
008200                                                                          
008300 77  UPDATE-W6A164               PIC X       VALUE 'N'.                   
008400     88  W6A164-OK                           VALUE 'Y'.                   
008500     88  W6A164-NOK                          VALUE 'N'.                   
008600                                                                          
008700 77  WDT1-CHECK-SW               PIC X(5)    VALUE 'XXXXX'.               
008800     88  WDT101-CHECK                        VALUE 'WDT10'.               
008900     88  WDT1A1-CHECK                        VALUE 'WDT1A'.               
009000     88  WDT1B1-CHECK                        VALUE 'WDT1B'.               
009100     88  WDT1C1-CHECK                        VALUE 'WDT1C'.               
009200     88  WDT1D1-CHECK                        VALUE 'WDT1D'.               
009300     88  WDT1E1-CHECK                        VALUE 'WDT1E'.               
009400     88  WDT1F1-CHECK                        VALUE 'WDT1F'.               
009500     88  WDT1G1-CHECK                        VALUE 'WDT1G'.               
009600     88  WDT1H1-CHECK                        VALUE 'WDT1H'.               
009700     88  WDT1I1-CHECK                        VALUE 'WDT1I'.               
009800                                                                          
009900 77  INDATA-SW                   PIC X       VALUE 'Y'.                   
010000     88  INDATA-OK                           VALUE 'Y'.                   
010100     88  INDATA-WRONG                        VALUE 'N'.                   
010200                                                                          
010300 77  JUMP-SW                     PIC X       VALUE 'N'.                   
010400     88  JUMP-OK                             VALUE 'Y'.                   
010500     88  JUMP-WRONG                          VALUE 'N'.                   
010600                                                                          
010700 77  JUMP-CHK-SW                 PIC X       VALUE 'N'.                   
010800     88  JUMP-CHK-OK                         VALUE 'Y'.                   
010900     88  JUMP-CHK-WRONG                      VALUE 'N'.                   
011000                                                                          
011100 77  CHK-CMD-C-SW                PIC X       VALUE 'Y'.                   
011200     88  CMD-C-CHK-OK                        VALUE 'Y'.                   
011300     88  CMD-C-CHK-WRONG                     VALUE 'N'.                   
011400                                                                          
011500 77  KEYS-SW                     PIC X       VALUE 'J'.                   
011600     88  KEYS-OK                             VALUE 'J'.                   
011700     88  KEYS-WRONG                          VALUE 'N'.                   
011800                                                                          
011900 77  ALLT-SW                     PIC X       VALUE 'Y'.                   
012000     88  ALLT-OK                             VALUE 'Y'.                   
012100     88  ALLT-NOT-OK                         VALUE 'N'.                   
012200                                                                          
012300 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
012400     88  OWN-MID                             VALUE '6164'.                
012500     88  GOOD-MID                            VALUE '6162'.                
012600     88  HELP-MID                            VALUE '0551'.                
012700     EJECT                                                                
012800** SUBPROGRAMS                                                            
012900 01  GENERAL-SUBPROGRAMS.                                                 
013000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
013100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
013200     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
013300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013400     03  W488ORCR                PIC X(8)    VALUE 'W488ORCR'.            
013410     03  W488ORCN                PIC X(8)    VALUE 'W488ORCN'.            
013500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013600     EJECT                                                                
013700*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
013800*01 -COPY WMEDAREA                                                        
013900     SKIP3                                                                
014000 01  MESSAGE-CODES.                                                       
014100     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
014200     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
014300     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
014400     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
014500     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
014600     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
014700     03  INF-NO-RECORDS          PIC X(3)    VALUE '010'.                 
014800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
014900     03  ERR-PART-NBR-MISSING    PIC X(3)    VALUE '017'.                 
015000     03  ERR-PRINT-NOT-FOUND     PIC X(3)    VALUE '772'.                 
015100     03  INF-PRINT-BEGAERD       PIC X(3)    VALUE '118'.                 
015200     EJECT                                                                
015300 01  SCREEN-MESSAGE.                                                      
015400     03 NO-PRINT-ENTER           PIC X(40)   VALUE                        
015500                                 'NO PRINTER ENTERED'.                    
015600*    --- PARAMETERS FOR PRINT SUBROUTINE W006PRT                          
015700*01  -COPY W006PRT                                                        
015800     EJECT                                                                
015900                                                                          
016000*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
016100 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
016200     SKIP3                                                                
016300*01 -COPY WMSGINIT                                                        
016400     EJECT                                                                
016500*    --- PARAMETERS FOR SUB PROGRAM W488ORCR                              
016600*                                                                         
016700 01  FILLER                      PIC X(16)   VALUE 'W488ORCR'.            
016800*01 -COPY W488ORCR                                                        
016900     EJECT                                                                
016910*    --- PARAMETERS FOR SUB PROGRAM W488ORCN                              
016920*                                                                         
016930 01  FILLER                      PIC X(16)   VALUE 'W488ORCN'.            
016940*01 -COPY W488ORCN                                                        
016950     EJECT                                                                
017000*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
017100*                                                                         
017200 01  SAVE-AREA.                                                           
017300     03  SAVE-IDTRANS             PIC X(4)    VALUE '6164'.               
017400     03  SAVE-IDUSER              PIC X(7)    VALUE SPACE.                
017500                                                                          
017600** SAVE AREA FOR WDT101 KEYS                                              
017700     03  SAVE-WDT101KY-ENTER.                                             
017800         05  SAVE-IDDC-WDT101-E       PIC X(2).                           
017900         05  SAVE-IDARTNR-WDT101-E    PIC S9(9)        COMP-3.            
018000         05  SAVE-TIORDTIME-WDT101-E  PIC 9(12).                          
018100                                                                          
018200     03  SAVE-WDT101KY-NEXT.                                              
018300         05  SAVE-IDDC-WDT101-N       PIC X(2).                           
018400         05  SAVE-IDARTNR-WDT101-N    PIC S9(9)        COMP-3.            
018500         05  SAVE-TIORDTIME-WDT101-N  PIC 9(12).                          
018600                                                                          
018700** SAVE AREA FOR WDT1A1 AND WDT1B1 KEYS                                   
018800     03  SAVE-AREA-ENTER.                                                 
018900         05  SAVE-IDDC-ENTER          PIC X(2).                           
019000         05  SAVE-ADLAGOMR-ADR-ENTER  PIC S9(3)        COMP-3.            
019100         05  SAVE-KDSTAPF-ENTER       PIC X(1).                           
019200         05  SAVE-KDPRIO-PF-ENTER     PIC S9(1)        COMP-3.            
019300         05  SAVE-TIORDTIME-ENTER     PIC 9(12).                          
019400                                                                          
019500     03  SAVE-AREA-NEXT.                                                  
019600         05  SAVE-IDDC-NEXT           PIC X(2).                           
019700         05  SAVE-ADLAGOMR-ADR-NEXT   PIC S9(3)        COMP-3.            
019800         05  SAVE-KDSTAPF-NEXT        PIC X(1).                           
019900         05  SAVE-KDPRIO-PF-NEXT      PIC S9(1)        COMP-3.            
020000         05  SAVE-TIORDTIME-NEXT      PIC 9(12).                          
020100                                                                          
020200** SAVE AREA FOR WDT1C1 KEYS                                              
020300     03  SAVE-WDT1C1KY-ENTER.                                             
020400         05  SAVE-IDDC-WDT1C-E       PIC X(2)  VALUE '11'.                
020500         05  SAVE-ADLAGFOM-WDT1C-E   PIC S9(3) COMP-3 VALUE ZERO.         
020600         05  SAVE-ADGANGFOM-WDT1C-E  PIC S9(3) COMP-3 VALUE ZERO.         
020700         05  SAVE-ADPLATFOM-WDT1C-E  PIC S9(5) COMP-3 VALUE ZERO.         
020800         05  SAVE-TIORDTIME-WDT1C-E  PIC 9(12) VALUE ZERO.                
020900                                                                          
021000     03  SAVE-WDT1C1KY-NEXT.                                              
021100         05  SAVE-IDDC-WDT1C-N       PIC X(2)  VALUE '11'.                
021200         05  SAVE-ADLAGFOM-WDT1C-N   PIC S9(3) COMP-3 VALUE ZERO.         
021300         05  SAVE-ADGANGFOM-WDT1C-N  PIC S9(3) COMP-3 VALUE ZERO.         
021400         05  SAVE-ADPLATFOM-WDT1C-N  PIC S9(5) COMP-3 VALUE ZERO.         
021500         05  SAVE-TIORDTIME-WDT1C-N  PIC 9(12) VALUE ZERO.                
021600                                                                          
021700** SAVE AREA FOR WDT1D1 KEYS                                              
021800     03  SAVE-WDT1D1KY-ENTER.                                             
021900         05  SAVE-IDDC-WDT1D-E      PIC X(2)  VALUE '11'.                 
022000         05  SAVE-ADLAGTOM-WDT1D-E  PIC S9(3) COMP-3 VALUE ZERO.          
022100         05  SAVE-ADGANGTOM-WDT1D-E PIC S9(3) COMP-3 VALUE ZERO.          
022200         05  SAVE-ADPLATTOM-WDT1D-E PIC S9(5) COMP-3 VALUE ZERO.          
022300         05  SAVE-TIORDTIME-WDT1D-E PIC 9(12) VALUE ZERO.                 
022400                                                                          
022500     03  SAVE-WDT1D1KY-NEXT.                                              
022600         05  SAVE-IDDC-WDT1D-N      PIC X(2)  VALUE '11'.                 
022700         05  SAVE-ADLAGTOM-WDT1D-N  PIC S9(3) COMP-3 VALUE ZERO.          
022800         05  SAVE-ADGANGTOM-WDT1D-N PIC S9(3) COMP-3 VALUE ZERO.          
022900         05  SAVE-ADPLATTOM-WDT1D-N PIC S9(5) COMP-3 VALUE ZERO.          
023000         05  SAVE-TIORDTIME-WDT1D-N PIC 9(12) VALUE ZERO.                 
023100                                                                          
023200** SAVE AREA FOR WDT1E1 KEYS                                              
023300     03  SAVE-WDT1E1KY-ENTER.                                             
023400         05  SAVE-IDDC-WDT1E-E      PIC X(2)  VALUE '11'.                 
023500         05  SAVE-IDUSER-WDT1E-E    PIC X(8)  VALUE LOW-VALUE.            
023600         05  SAVE-ADLAGFOM-WDT1E-E  PIC S9(3) COMP-3 VALUE ZERO.          
023700         05  SAVE-ADGANGFOM-WDT1E-E PIC S9(3) COMP-3 VALUE ZERO.          
023800         05  SAVE-ADPLATFOM-WDT1E-E PIC S9(5) COMP-3 VALUE ZERO.          
023900         05  SAVE-KDSTAPF-WDT1E-E   PIC X(1) VALUE LOW-VALUE.             
024000         05  SAVE-TIORDTIME-WDT1E-E PIC 9(12) VALUE ZERO.                 
024100                                                                          
024200     03  SAVE-WDT1E1KY-NEXT.                                              
024300         05  SAVE-IDDC-WDT1E-N      PIC X(2)  VALUE '11'.                 
024400         05  SAVE-IDUSER-WDT1E-N    PIC X(8)  VALUE LOW-VALUE.            
024500         05  SAVE-ADLAGFOM-WDT1E-N  PIC S9(3) COMP-3 VALUE ZERO.          
024600         05  SAVE-ADGANGFOM-WDT1E-N PIC S9(3) COMP-3 VALUE ZERO.          
024700         05  SAVE-ADPLATFOM-WDT1E-N PIC S9(5) COMP-3 VALUE ZERO.          
024800         05  SAVE-KDSTAPF-WDT1E-N   PIC X(1) VALUE LOW-VALUE.             
024900         05  SAVE-TIORDTIME-WDT1E-N PIC 9(12) VALUE ZERO.                 
025000                                                                          
025100** SAVE AREA FOR WDT1F1 KEYS                                              
025200     03  SAVE-WDT1F1KY-ENTER.                                             
025300         05  SAVE-IDDC-WDT1F-E      PIC X(2)  VALUE '11'.                 
025400         05  SAVE-IDUSER-WDT1F-E    PIC X(8)  VALUE LOW-VALUE.            
025500         05  SAVE-ADLAGTOM-WDT1F-E  PIC S9(3) COMP-3 VALUE ZERO.          
025600         05  SAVE-ADGANGTOM-WDT1F-E PIC S9(3) COMP-3 VALUE ZERO.          
025700         05  SAVE-ADPLATTOM-WDT1F-E PIC S9(5) COMP-3 VALUE ZERO.          
025800         05  SAVE-KDSTAPF-WDT1F-E   PIC X(1)  VALUE LOW-VALUE.            
025900         05  SAVE-TIORDTIME-WDT1F-E PIC 9(12) VALUE ZERO.                 
026000                                                                          
026100     03  SAVE-WDT1F1KY-NEXT.                                              
026200         05  SAVE-IDDC-WDT1F-N      PIC X(2)  VALUE '11'.                 
026300         05  SAVE-IDUSER-WDT1F-N    PIC X(8)  VALUE LOW-VALUE.            
026400         05  SAVE-ADLAGTOM-WDT1F-N  PIC S9(3) COMP-3 VALUE ZERO.          
026500         05  SAVE-ADGANGTOM-WDT1F-N PIC S9(3) COMP-3 VALUE ZERO.          
026600         05  SAVE-ADPLATTOM-WDT1F-N PIC S9(5) COMP-3 VALUE ZERO.          
026700         05  SAVE-KDSTAPF-WDT1F-N   PIC X(1)  VALUE LOW-VALUE.            
026800         05  SAVE-TIORDTIME-WDT1F-N PIC 9(12) VALUE ZERO.                 
026900                                                                          
027000** SAVE AREA FOR WDT1G1 KEYS                                              
027100     03  SAVE-WDT1G1KY-ENTER.                                             
027200         05  SAVE-IDDC-WDT1G-E      PIC X(2)  VALUE '11'.                 
027300         05  SAVE-KDPRIO-WDT1G-E    PIC S9    COMP-3 VALUE ZERO.          
027400         05  SAVE-ADLAGFOM-WDT1G-E  PIC S9(3) COMP-3 VALUE ZERO.          
027500         05  SAVE-ADGANGFOM-WDT1G-E PIC S9(3) COMP-3 VALUE ZERO.          
027600         05  SAVE-ADPLATFOM-WDT1G-E PIC S9(5) COMP-3 VALUE ZERO.          
027700         05  SAVE-KDSTAPF-WDT1G-E   PIC X(1) VALUE LOW-VALUE.             
027800         05  SAVE-TIORDTIME-WDT1G-E PIC 9(12) VALUE ZERO.                 
027900                                                                          
028000     03  SAVE-WDT1G1KY-NEXT.                                              
028100         05  SAVE-IDDC-WDT1G-N      PIC X(2)  VALUE '11'.                 
028200         05  SAVE-KDPRIO-WDT1G-N    PIC S9    COMP-3 VALUE ZERO.          
028300         05  SAVE-ADLAGFOM-WDT1G-N  PIC S9(3) COMP-3 VALUE ZERO.          
028400         05  SAVE-ADGANGFOM-WDT1G-N PIC S9(3) COMP-3 VALUE ZERO.          
028500         05  SAVE-ADPLATFOM-WDT1G-N PIC S9(5) COMP-3 VALUE ZERO.          
028600         05  SAVE-KDSTAPF-WDT1G-N   PIC X(1) VALUE LOW-VALUE.             
028700         05  SAVE-TIORDTIME-WDT1G-N PIC 9(12) VALUE ZERO.                 
028800                                                                          
028900** SAVE AREA FOR WDT1H1 KEYS                                              
029000     03  SAVE-WDT1H1KY-ENTER.                                             
029100         05  SAVE-IDDC-WDT1H-E      PIC X(2)  VALUE '11'.                 
029200         05  SAVE-KDPRIO-WDT1H-E    PIC S9    COMP-3 VALUE ZERO.          
029300         05  SAVE-ADLAGTOM-WDT1H-E  PIC S9(3) COMP-3 VALUE ZERO.          
029400         05  SAVE-ADGANGTOM-WDT1H-E PIC S9(3) COMP-3 VALUE ZERO.          
029500         05  SAVE-ADPLATTOM-WDT1H-E PIC S9(5) COMP-3 VALUE ZERO.          
029600         05  SAVE-KDSTAPF-WDT1H-E   PIC X(1) VALUE LOW-VALUE.             
029700         05  SAVE-TIORDTIME-WDT1H-E PIC 9(12) VALUE ZERO.                 
029800                                                                          
029900     03  SAVE-WDT1H1KY-NEXT.                                              
030000         05  SAVE-IDDC-WDT1H-N      PIC X(2)  VALUE '11'.                 
030100         05  SAVE-KDPRIO-WDT1H-N    PIC S9    COMP-3 VALUE ZERO.          
030200         05  SAVE-ADLAGTOM-WDT1H-N  PIC S9(3) COMP-3 VALUE ZERO.          
030300         05  SAVE-ADGANGTOM-WDT1H-N PIC S9(3) COMP-3 VALUE ZERO.          
030400         05  SAVE-ADPLATTOM-WDT1H-N PIC S9(5) COMP-3 VALUE ZERO.          
030500         05  SAVE-KDSTAPF-WDT1H-N   PIC X(1) VALUE LOW-VALUE.             
030600         05  SAVE-TIORDTIME-WDT1H-N PIC 9(12) VALUE ZERO.                 
030700                                                                          
030800** SAVE AREA FOR WDT1I1 KEYS                                              
030900     03  SAVE-WDT1I1KY-ENTER.                                             
031000         05  SAVE-IDDC-WDT1I-E      PIC X(2)  VALUE '11'.                 
031100         05  SAVE-ADLAGFOM-WDT1I-E  PIC S9(3) COMP-3 VALUE ZERO.          
031200         05  SAVE-ADLAGTOM-WDT1I-E  PIC S9(3) COMP-3 VALUE ZERO.          
031300         05  SAVE-KDPRIO-WDT1I-E    PIC S9    COMP-3 VALUE ZERO.          
031400         05  SAVE-ADGANGFOM-WDT1I-E PIC S9(3) COMP-3 VALUE ZERO.          
031500         05  SAVE-ADPLATFOM-WDT1I-E PIC S9(5) COMP-3 VALUE ZERO.          
031600         05  SAVE-KDSTAPF-WDT1I-E   PIC X(1)  VALUE LOW-VALUE.            
031700         05  SAVE-TIORDTIME-WDT1I-E PIC 9(12) VALUE ZERO.                 
031800                                                                          
031900     03  SAVE-WDT1I1KY-NEXT.                                              
032000         05  SAVE-IDDC-WDT1I-N      PIC X(2)  VALUE '11'.                 
032100         05  SAVE-ADLAGFOM-WDT1I-N  PIC S9(3) COMP-3 VALUE ZERO.          
032200         05  SAVE-ADLAGTOM-WDT1I-N  PIC S9(3) COMP-3 VALUE ZERO.          
032300         05  SAVE-KDPRIO-WDT1I-N    PIC S9    COMP-3 VALUE ZERO.          
032400         05  SAVE-ADGANGFOM-WDT1I-N PIC S9(3) COMP-3 VALUE ZERO.          
032500         05  SAVE-ADPLATFOM-WDT1I-N PIC S9(5) COMP-3 VALUE ZERO.          
032600         05  SAVE-KDSTAPF-WDT1I-N   PIC X(1)  VALUE LOW-VALUE.            
032700         05  SAVE-TIORDTIME-WDT1I-N PIC 9(12) VALUE ZERO.                 
032800                                                                          
032900     EJECT                                                                
033000*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
033100*                                                                         
033200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
033300     SKIP3                                                                
033400*01  MID -COPY W6I16401                                                   
033500*01  MID -COPY W6I16402                                                   
033600     EJECT                                                                
033700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
033800     SKIP3                                                                
033900*01  -COPY WMSGAREA                                                       
034000     EJECT                                                                
034100     03  MOD REDEFINES MSG-AREA.                                          
034200*      05  -COPY W6O16401                                                 
034300     EJECT                                                                
034400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
034500     SKIP3                                                                
034600*01  -COPY WMFSAREA                                                       
034700     EJECT                                                                
034800                                                                          
034900*PRINTER                                                                  
035000 01  PROG-TO-PROG-SW1.                                                    
035100*    03    -COPY  WMSGSOP                                                 
035200     EJECT                                                                
035300 01  WS-IDPRT.                                                            
035400     03  WS-IDPRT1            PIC X(2).                                   
035500     03  WS-IDPRT2            PIC X(4).                                   
035600     03  WS-IDPRT3            PIC X(2).                                   
035700                                                                          
035800 01  WS-PARAMETRAR.                                                       
035900     03  WS-URVAL.                                                        
036000         05  URV-ADLAGOMR-FOM    PIC X(2) VALUE ZERO.                     
036100         05  URV-ADGANG-FOM      PIC X(2) VALUE ZERO.                     
036200         05  URV-ADLAGOMR-TOM    PIC X(2) VALUE ZERO.                     
036300         05  URV-ADGANG-TOM      PIC X(2) VALUE ZERO.                     
036400         05  URV-KDSTAPF         PIC X(1) VALUE SPACE.                    
036500         05  URV-IDARTNR         PIC X(9) VALUE SPACE.                    
036600         05  URV-IDUSER          PIC X(7) VALUE SPACE.                    
036700         05  URV-KDPRIO-PF       PIC X(1) VALUE ZERO.                     
036800     03  WS-PRINTER.                                                      
036900         05  URV-IDPRINTER       PIC X(8) VALUE SPACE.                    
037000                                                                          
037100 01  W-PROG-TO-PROG-SW2.                                                  
037200     03  M-SW-LL                 PIC S9(4)   VALUE +240 COMP SYNC.        
037300     03  M-SW-Z1-Z2              PIC X(2)    VALUE LOW-VALUE.             
037400     03  M-SW-KDTRANS            PIC X(8)    VALUE 'W6T162  '.            
037500     03  M-SW-IDTRANS            PIC X(4)    VALUE '6162'.                
037600     03  M-SW-KDMFSTYP           PIC X(1)    VALUE '2'.                   
037700                                                                          
037800*    03  MID -COPY W6I16201 -PRE 6162-                                    
037900                                                                          
038000     EJECT                                                                
038100*    --- WORK-AREAS FOR IMS-SECTIONS                                      
038200*                                                                         
038300 01  FILLER                    PIC X(16)   VALUE 'IMS-WS'.                
038400     SKIP3                                                                
038500 01  KEYS-FOR-DLI.                                                        
038600*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
038700     03  W-IDDC-MIN-X.                                                    
038800         05  W-IDDC-MIN        PIC X(2).                                  
038900                                                                          
039000     03  W-IDARTNR-MIN-X.                                                 
039100         05  W-IDARTNR-MIN     PIC S9(9)        COMP-3.                   
039200                                                                          
039300     03  W-TIORDTIME-MIN-X.                                               
039400         05  W-TIORDTIME-MIN   PIC S9(12).                                
039500                                                                          
039600     03  W-ADLAGOMR-FOM-X.                                                
039700         05  W-ADLAGOMR-FOM-N  PIC S9(3) COMP-3 VALUE ZERO.               
039800                                                                          
039900     03  W-ADLAGOMR-TOM-X.                                                
040000         05  W-ADLAGOMR-TOM-N  PIC S9(3) COMP-3 VALUE ZERO.               
040100                                                                          
040200** KEYS DECLARATION FOR WDT101                                            
040300     03  W-WDT101KY-X.                                                    
040400         05  W-IDDC-WDT101        PIC X(2)  VALUE '11'.                   
040500         05  W-IDARTNR-WDT101     PIC S9(9) COMP-3 VALUE ZERO.            
040600         05  W-TIORDTIME-WDT101   PIC 9(12) VALUE ZERO.                   
040700                                                                          
040800** KEYS DECLARATION FOR WDT101 - MIN AND MAX                              
040900     03  W-WDT101KY-MIN-X.                                                
041000         05  W-IDDC-WDT101-MIN      PIC X(2)  VALUE '11'.                 
041100         05  W-IDARTNR-WDT101-MIN   PIC S9(9) COMP-3 VALUE ZERO.          
041200         05  W-TIORDTIME-WDT101-MIN PIC 9(12) VALUE ZERO.                 
041300                                                                          
041400     03  W-WDT101KY-MAX-X.                                                
041500         05  W-IDDC-WDT101-MAX      PIC X(2)  VALUE '11'.                 
041600         05  W-IDARTNR-WDT101-MAX   PIC S9(9) COMP-3                      
041700                                              VALUE +999999999.           
041800         05  W-TIORDTIME-WDT101-MAX PIC 9(12)                             
041900                                              VALUE 999999999999.         
042000** KEYS DECLARATION FOR WDT1A1                                            
042100     03  W-WDT1A1KY-MIN-X.                                                
042200         05  W-IDDC-WDT1A-MIN      PIC X(2)  VALUE '11'.                  
042300         05  W-ADLAGFOM-WDT1A-MIN  PIC S9(3) COMP-3 VALUE ZERO.           
042400         05  W-KDSTAPF-WDT1A-MIN   PIC X(1)  VALUE LOW-VALUE.             
042500         05  W-KDPRIO-PF-WDT1A-MIN PIC  S9(1) COMP-3 VALUE ZERO.          
042600         05  W-TIORDTIME-WDT1A-MIN PIC  9(12) VALUE ZERO.                 
042700                                                                          
042800     03  W-WDT1A1KY-MAX-X.                                                
042900         05  W-IDDC-WDT1A-MAX      PIC X(2)  VALUE '11'.                  
043000         05  W-ADLAGFOM-WDT1A-MAX  PIC S9(3) COMP-3 VALUE +999.           
043100         05  W-KDSTAPF-WDT1A-MAX   PIC X(1)  VALUE HIGH-VALUE.            
043200         05  W-KDPRIO-PF-WDT1A-MAX PIC S9(1) COMP-3 VALUE +9.             
043300         05  W-TIORDTIME-WDT1A-MAX PIC 9(12) VALUE 999999999999.          
043400                                                                          
043500** KEYS DECLARATION FOR WDT1B1                                            
043600     03  W-WDT1B1KY-MIN-X.                                                
043700         05  W-IDDC-WDT1B-MIN      PIC X(2)  VALUE '11'.                  
043800         05  W-ADLAGTOM-WDT1B-MIN  PIC S9(3) COMP-3 VALUE ZERO.           
043900         05  W-KDSTAPF-WDT1B-MIN   PIC X(1)  VALUE LOW-VALUE.             
044000         05  W-KDPRIO-PF-WDT1B-MIN PIC S9(1) COMP-3 VALUE ZERO.           
044100         05  W-TIORDTIME-WDT1B-MIN PIC 9(12) VALUE ZERO.                  
044200                                                                          
044300     03  W-WDT1B1KY-MAX-X.                                                
044400         05  W-IDDC-WDT1B-MAX      PIC X(2)  VALUE '11'.                  
044500         05  W-ADLAGTOM-WDT1B-MAX  PIC S9(3) COMP-3 VALUE +999.           
044600         05  W-KDSTAPF-WDT1B-MAX   PIC X(1)  VALUE HIGH-VALUE.            
044700         05  W-KDPRIO-PF-WDT1B-MAX PIC S9(1) COMP-3 VALUE +9.             
044800         05  W-TIORDTIME-WDT1B-MAX PIC 9(12) VALUE 999999999999.          
044900                                                                          
045000** KEYS DECLARATION FOR WDT1C1                                            
045100     03  W-WDT1C1KY-MIN-X.                                                
045200         05  W-IDDC-WDT1C-MIN      PIC X(2)  VALUE '11'.                  
045300         05  W-ADLAGFOM-WDT1C-MIN  PIC S9(3) COMP-3 VALUE ZERO.           
045400         05  W-ADGANGFOM-WDT1C-MIN PIC S9(3) COMP-3 VALUE ZERO.           
045500         05  W-ADPLATFOM-WDT1C-MIN PIC S9(5) COMP-3 VALUE ZERO.           
045600         05  W-TIORDTIME-WDT1C-MIN PIC 9(12) VALUE ZERO.                  
045700                                                                          
045800     03  W-WDT1C1KY-MAX-X.                                                
045900         05  W-IDDC-WDT1C-MAX      PIC X(2)  VALUE '11'.                  
046000         05  W-ADLAGFOM-WDT1C-MAX  PIC S9(3) COMP-3 VALUE +999.           
046100         05  W-ADGANGFOM-WDT1C-MAX PIC S9(3) COMP-3 VALUE +999.           
046200         05  W-ADPLATFOM-WDT1C-MAX PIC S9(5) COMP-3 VALUE +99999.         
046300         05  W-TIORDTIME-WDT1C-MAX PIC 9(12) VALUE 999999999999.          
046400                                                                          
046500** KEYS DECLARATION FOR WDT1D1                                            
046600     03  W-WDT1D1KY-MIN-X.                                                
046700         05  W-IDDC-WDT1D-MIN      PIC X(2)  VALUE '11'.                  
046800         05  W-ADLAGTOM-WDT1D-MIN  PIC S9(3) COMP-3 VALUE ZERO.           
046900         05  W-ADGANGTOM-WDT1D-MIN PIC S9(3) COMP-3 VALUE ZERO.           
047000         05  W-ADPLATTOM-WDT1D-MIN PIC S9(5) COMP-3 VALUE ZERO.           
047100         05  W-TIORDTIME-WDT1D-MIN PIC 9(12) VALUE ZERO.                  
047200                                                                          
047300     03  W-WDT1D1KY-MAX-X.                                                
047400         05  W-IDDC-WDT1D-MAX      PIC X(2)  VALUE '11'.                  
047500         05  W-ADLAGTOM-WDT1D-MAX  PIC S9(3) COMP-3 VALUE +999.           
047600         05  W-ADGANGTOM-WDT1D-MAX PIC S9(3) COMP-3 VALUE +999.           
047700         05  W-ADPLATTOM-WDT1D-MAX PIC S9(5) COMP-3 VALUE +99999.         
047800         05  W-TIORDTIME-WDT1D-MAX PIC 9(12) VALUE 999999999999.          
047900                                                                          
048000** KEYS DECLARATION FOR WDT1E1                                            
048100     03  W-WDT1E1KY-MIN-X.                                                
048200         05  W-IDDC-WDT1E-MIN      PIC X(2)  VALUE '11'.                  
048300         05  W-IDUSER-WDT1E-MIN    PIC X(8)  VALUE LOW-VALUE.             
048400         05  W-ADLAGFOM-WDT1E-MIN  PIC S9(3) COMP-3 VALUE ZERO.           
048500         05  W-ADGANGFOM-WDT1E-MIN PIC S9(3) COMP-3 VALUE ZERO.           
048600         05  W-ADPLATFOM-WDT1E-MIN PIC S9(5) COMP-3 VALUE ZERO.           
048700         05  W-KDSTAPF-WDT1E-MIN   PIC X(1)  VALUE LOW-VALUE.             
048800         05  W-TIORDTIME-WDT1E-MIN PIC 9(12) VALUE ZERO.                  
048900                                                                          
049000     03  W-WDT1E1KY-MAX-X.                                                
049100         05  W-IDDC-WDT1E-MAX      PIC X(2)  VALUE '11'.                  
049200         05  W-IDUSER-WDT1E-MAX    PIC X(8)  VALUE HIGH-VALUE.            
049300         05  W-ADLAGFOM-WDT1E-MAX  PIC S9(3) COMP-3 VALUE 999.            
049400         05  W-ADGANGFOM-WDT1E-MAX PIC S9(3) COMP-3 VALUE 999.            
049500         05  W-ADPLATFOM-WDT1E-MAX PIC S9(5) COMP-3 VALUE 99999.          
049600         05  W-KDSTAPF-WDT1E-MAX   PIC X(1)  VALUE HIGH-VALUE.            
049700         05  W-TIORDTIME-WDT1E-MAX PIC 9(12) VALUE 999999999999.          
049800                                                                          
049900** KEYS DECLARATION FOR WDT1F1                                            
050000     03  W-WDT1F1KY-MIN-X.                                                
050100         05  W-IDDC-WDT1F-MIN      PIC X(2)  VALUE '11'.                  
050200         05  W-IDUSER-WDT1F-MIN    PIC X(8)  VALUE LOW-VALUE.             
050300         05  W-ADLAGTOM-WDT1F-MIN  PIC S9(3) COMP-3 VALUE ZERO.           
050400         05  W-ADGANGTOM-WDT1F-MIN PIC S9(3) COMP-3 VALUE ZERO.           
050500         05  W-ADPLATTOM-WDT1F-MIN PIC S9(5) COMP-3 VALUE ZERO.           
050600         05  W-KDSTAPF-WDT1F-MIN   PIC X(1)  VALUE LOW-VALUE.             
050700         05  W-TIORDTIME-WDT1F-MIN PIC 9(12) VALUE ZERO.                  
050800                                                                          
050900     03  W-WDT1F1KY-MAX-X.                                                
051000         05  W-IDDC-WDT1F-MAX      PIC X(2)  VALUE '11'.                  
051100         05  W-IDUSER-WDT1F-MAX    PIC X(8)  VALUE HIGH-VALUE.            
051200         05  W-ADLAGTOM-WDT1F-MAX  PIC S9(3) COMP-3 VALUE 999.            
051300         05  W-ADGANGTOM-WDT1F-MAX PIC S9(3) COMP-3 VALUE 999.            
051400         05  W-ADPLATTOM-WDT1F-MAX PIC S9(5) COMP-3 VALUE 99999.          
051500         05  W-KDSTAPF-WDT1F-MAX   PIC X(1)  VALUE HIGH-VALUE.            
051600         05  W-TIORDTIME-WDT1F-MAX PIC 9(12) VALUE 999999999999.          
051700                                                                          
051800** KEYS DECLARATION FOR WDT1G1                                            
051900     03  W-WDT1G1KY-MIN-X.                                                
052000         05  W-IDDC-WDT1G-MIN      PIC X(2)  VALUE '11'.                  
052100         05  W-KDPRIO-WDT1G-MIN    PIC S9    COMP-3 VALUE ZERO.           
052200         05  W-ADLAGFOM-WDT1G-MIN  PIC S9(3) COMP-3 VALUE ZERO.           
052300         05  W-ADGANGFOM-WDT1G-MIN PIC S9(3) COMP-3 VALUE ZERO.           
052400         05  W-ADPLATFOM-WDT1G-MIN PIC S9(5) COMP-3 VALUE ZERO.           
052500         05  W-KDSTAPF-WDT1G-MIN   PIC X(1)  VALUE LOW-VALUE.             
052600         05  W-TIORDTIME-WDT1G-MIN PIC 9(12) VALUE ZERO.                  
052700                                                                          
052800     03  W-WDT1G1KY-MAX-X.                                                
052900         05  W-IDDC-WDT1G-MAX      PIC X(2)  VALUE '11'.                  
053000         05  W-KDPRIO-WDT1G-MAX    PIC S9    COMP-3 VALUE 9.              
053100         05  W-ADLAGFOM-WDT1G-MAX  PIC S9(3) COMP-3 VALUE 999.            
053200         05  W-ADGANGFOM-WDT1G-MAX PIC S9(3) COMP-3 VALUE 999.            
053300         05  W-ADPLATFOM-WDT1G-MAX PIC S9(5) COMP-3 VALUE 99999.          
053400         05  W-KDSTAPF-WDT1G-MAX   PIC X(1)  VALUE HIGH-VALUE.            
053500         05  W-TIORDTIME-WDT1G-MAX PIC 9(12) VALUE 999999999999.          
053600                                                                          
053700** KEYS DECLARATION FOR WDT1H1                                            
053800     03  W-WDT1H1KY-MIN-X.                                                
053900         05  W-IDDC-WDT1H-MIN      PIC X(2)  VALUE '11'.                  
054000         05  W-KDPRIO-WDT1H-MIN    PIC S9    COMP-3 VALUE ZERO.           
054100         05  W-ADLAGTOM-WDT1H-MIN  PIC S9(3) COMP-3 VALUE ZERO.           
054200         05  W-ADGANGTOM-WDT1H-MIN PIC S9(3) COMP-3 VALUE ZERO.           
054300         05  W-ADPLATTOM-WDT1H-MIN PIC S9(5) COMP-3 VALUE ZERO.           
054400         05  W-KDSTAPF-WDT1H-MIN   PIC X(1)  VALUE LOW-VALUE.             
054500         05  W-TIORDTIME-WDT1H-MIN PIC 9(12) VALUE ZERO.                  
054600                                                                          
054700     03  W-WDT1H1KY-MAX-X.                                                
054800         05  W-IDDC-WDT1H-MAX      PIC X(2)  VALUE '11'.                  
054900         05  W-KDPRIO-WDT1H-MAX    PIC S9    COMP-3 VALUE 9.              
055000         05  W-ADLAGTOM-WDT1H-MAX  PIC S9(3) COMP-3 VALUE 999.            
055100         05  W-ADGANGTOM-WDT1H-MAX PIC S9(3) COMP-3 VALUE 999.            
055200         05  W-ADPLATTOM-WDT1H-MAX PIC S9(5) COMP-3 VALUE 99999.          
055300         05  W-KDSTAPF-WDT1H-MAX   PIC X(1)  VALUE HIGH-VALUE.            
055400         05  W-TIORDTIME-WDT1H-MAX PIC 9(12) VALUE 999999999999.          
055500                                                                          
055600** KEYS DECLARATION FOR WDT1I1                                            
055700     03  W-WDT1I1KY-MIN-X.                                                
055800         05  W-IDDC-WDT1I-MIN      PIC X(2)  VALUE '11'.                  
055900         05  W-ADLAGFOM-WDT1I-MIN  PIC S9(3) COMP-3 VALUE ZERO.           
056000         05  W-ADLAGTOM-WDT1I-MIN  PIC S9(3) COMP-3 VALUE ZERO.           
056100         05  W-KDPRIO-WDT1I-MIN    PIC S9(1) COMP-3 VALUE ZERO.           
056200         05  W-ADGANGFOM-WDT1I-MIN PIC S9(3) COMP-3 VALUE ZERO.           
056300         05  W-ADPLATFOM-WDT1I-MIN PIC S9(5) COMP-3 VALUE ZERO.           
056400         05  W-KDSTAPF-WDT1I-MIN   PIC X(1)  VALUE LOW-VALUE.             
056500         05  W-TIORDTIME-WDT1I-MIN PIC 9(12) VALUE ZERO.                  
056600                                                                          
056700     03  W-WDT1I1KY-MAX-X.                                                
056800         05  W-IDDC-WDT1I-MAX      PIC X(2)  VALUE '11'.                  
056900         05  W-ADLAGFOM-WDT1I-MAX  PIC S9(3) COMP-3 VALUE ZERO.           
057000         05  W-ADLAGTOM-WDT1I-MAX  PIC S9(3) COMP-3 VALUE ZERO.           
057100         05  W-KDPRIO-WDT1I-MAX    PIC S9(1) COMP-3 VALUE +9.             
057200         05  W-ADGANGFOM-WDT1I-MAX PIC S9(3) COMP-3 VALUE ZERO.           
057300         05  W-ADPLATFOM-WDT1I-MAX PIC S9(5) COMP-3 VALUE ZERO.           
057400         05  W-KDSTAPF-WDT1I-MAX   PIC X(1)  VALUE LOW-VALUE.             
057500         05  W-TIORDTIME-WDT1I-MAX PIC 9(12) VALUE ZERO.                  
057600                                                                          
057700** KEYS DECLARATION FOR WDK601                                            
057800     03  W-IDARTNR-X.                                                     
057900         05  W-IDARTNR            PIC S9(9) COMP-3 VALUE ZERO.            
058000                                                                          
058100** KEYS DECLARATION FOR WDK611                                            
058200     03  W-KDSEGKEY-X.                                                    
058300         05  W-KDSEGKEY           PIC X(1)  VALUE '1'.                    
058400                                                                          
058500** KEYS DECLARATION FOR WDK811                                            
058600     03  W-WDD811KY-MIN-X.                                                
058700         05  W-IDDC-WDD8-MIN      PIC X(2)         VALUE '11'.            
058800         05  W-ADBUFFOM-WDD8-MIN  PIC S9(3) COMP-3 VALUE ZERO.            
058900         05  W-DABUFPAF-WDD8-MIN  PIC 9(8)         VALUE ZERO.            
059000         05  W-ADBUFGAN-WDD8-MIN  PIC S9(3) COMP-3 VALUE ZERO.            
059100         05  W-ADBUFPL-WDD8-MIN   PIC S9(5) COMP-3 VALUE ZERO.            
059200                                                                          
059300     03  W-WDD811KY-MAX-X.                                                
059400         05  W-IDDC-WDD8-MAX      PIC X(2)         VALUE '11'.            
059500         05  W-ADBUFFOM-WDD8-MAX  PIC S9(3) COMP-3 VALUE +999.            
059600         05  W-DABUFPAF-WDD8-MAX  PIC 9(8)         VALUE ZERO.            
059700         05  W-ADBUFGAN-WDD8-MAX  PIC S9(3) COMP-3 VALUE +999.            
059800         05  W-ADBUFPL-WDD8-MAX   PIC S9(5) COMP-3 VALUE +99999.          
059900                                                                          
060000** GENERAL KEYS                                                           
060100     03  W-ADBUFFOM-X.                                                    
060200         05  W-ADBUFFOM           PIC S9(3) COMP-3 VALUE ZERO.            
060300                                                                          
060400     03  W-ADBUFGAN-X.                                                    
060500         05  W-ADBUFGAN           PIC S9(3) COMP-3 VALUE ZERO.            
060600                                                                          
060700     03  W-ADBUFPL-X.                                                     
060800         05  W-ADBUFPL            PIC S9(5) COMP-3 VALUE ZERO.            
060900                                                                          
061000     03  W-IDDC-X.                                                        
061100         05  W-IDDC               PIC X(2)   VALUE '11'.                  
061200     SKIP2                                                                
061300*    --- STATUS CODES FROM IMS                                            
061400 01  STATUS-WS                    PIC XX.                                 
061500     88  SEGMENT-FOUND                       VALUE '  '.                  
061600     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
061700     88  SEGMENT-MISSING                     VALUE 'GE'.                  
061800     SKIP2                                                                
061900 01  GOOD-STATUSCODES.                                                    
062000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
062100     SKIP3                                                                
062200 01  SSA1                         PIC X(192).                             
062300 01  SSA2                         PIC X(128).                             
062400                                                                          
062500 01  TODAYS-DATE                  PIC 9(6)    VALUE ZERO.                 
062600 01  TODAYS-TIME                  PIC 9(8)    VALUE ZERO.                 
062700 01  FILLER REDEFINES TODAYS-TIME.                                        
062800     03  TODAYS-TIME-HHMMSS       PIC 9(6).                               
062900     03  TODAYS-TIME-HD           PIC 9(2).                               
063000     EJECT                                                                
063100*    --- IMS FUNCTION CODES                                               
063200*01  -COPY W0003                                                          
063300     EJECT                                                                
063400*    ---  DLI INPUT-OUTPUT AREA                                           
063500                                                                          
063600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT101'.                      
063700 01  DLI-IO-WDT101.                                                       
063800*    03  -COPY WDT101                                                     
063900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT1A1'.                      
064000 01  DLI-IO-WDT1A1.                                                       
064100*    03  -COPY WDT1A1                                                     
064200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT1B1'.                      
064300 01  DLI-IO-WDT1B1.                                                       
064400*    03  -COPY WDT1B1                                                     
064500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT1C1'.                      
064600 01  DLI-IO-WDT1C1.                                                       
064700*    03  -COPY WDT1C1                                                     
064800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT1D1'.                      
064900 01  DLI-IO-WDT1D1.                                                       
065000*    03  -COPY WDT1D1                                                     
065100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT1E1'.                      
065200 01  DLI-IO-WDT1E1.                                                       
065300*    03  -COPY WDT1E1                                                     
065400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT1F1'.                      
065500 01  DLI-IO-WDT1F1.                                                       
065600*    03  -COPY WDT1F1                                                     
065700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT1G1'.                      
065800 01  DLI-IO-WDT1G1.                                                       
065900*    03  -COPY WDT1G1                                                     
066000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT1H1'.                      
066100 01  DLI-IO-WDT1H1.                                                       
066200*    03  -COPY WDT1H1                                                     
066300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT1I1'.                      
066400 01  DLI-IO-WDT1I1.                                                       
066500*    03  -COPY WDT1I1                                                     
066600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
066700 01  DLI-IO-WDK611.                                                       
066800*    03  -COPY WDK611                                                     
066900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD811'.                      
067000 01  DLI-IO-WDD811.                                                       
067100*    03  -COPY WDD811                                                     
067200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDJ9'.                        
067300 01  DLI-IO-WDJ9.                                                         
067400*    03  -COPY WDJ911                                                     
067500     EJECT                                                                
067600 LINKAGE SECTION.                                                         
067700*01  -COPY W0009   -PRE MSG-                                              
067800                                                                          
067900*01  -COPY W0009   -PRE ALT1-                                             
068000                                                                          
068100*01  -COPY W0009   -PRE ALT2-                                             
068200                                                                          
068300*01  -COPY W0009  -PRE SYNQ-                                              
068400                                                                          
068500*01  -COPY W0008   -PRE USEA-                                             
068600     05  FILLER                  PIC X.                                   
068700                                                                          
068800*01  -COPY W0008  -PRE WDT1-                                              
068900     05  FILLER                  PIC X.                                   
069000                                                                          
069100*01  -COPY W0008  -PRE WDT1A-                                             
069200     05  FILLER                  PIC X.                                   
069300                                                                          
069400*01  -COPY W0008  -PRE WDT1B-                                             
069500     05  FILLER                  PIC X.                                   
069600                                                                          
069700*01  -COPY W0008  -PRE WDT1C-                                             
069800     05  FILLER                  PIC X.                                   
069900                                                                          
070000*01  -COPY W0008  -PRE WDT1D-                                             
070100     05  FILLER                  PIC X.                                   
070200                                                                          
070300*01  -COPY W0008  -PRE WDT1E-                                             
070400     05  FILLER                  PIC X.                                   
070500                                                                          
070600*01  -COPY W0008  -PRE WDT1F-                                             
070700     05  FILLER                  PIC X.                                   
070800                                                                          
070900*01  -COPY W0008  -PRE WDT1G-                                             
071000     05  FILLER                  PIC X.                                   
071100                                                                          
071200*01  -COPY W0008  -PRE WDT1H-                                             
071300     05  FILLER                  PIC X.                                   
071400                                                                          
071500*01  -COPY W0008  -PRE WDT1I-                                             
071600     05  FILLER                  PIC X.                                   
071700                                                                          
071800*01  -COPY W0008  -PRE WDK6-                                              
071900     05  FILLER                  PIC X.                                   
072000                                                                          
072100*01  -COPY W0008  -PRE WDD8-                                              
072200     05  FILLER                  PIC X.                                   
072300*01  -COPY W0008  -PRE WDJ9-                                              
072400     05  FILLER                  PIC X.                                   
072500 01  SYNQ-ATAB-PCB             PIC X.                                     
072510 01  WDQ3-PCB                  PIC X.                                     
072600                                                                          
072700     EJECT                                                                
072800 PROCEDURE DIVISION  USING MSG-PCB ALT1-PCB ALT2-PCB SYNQ-PCB             
072900     USEA-PCB WDT1-PCB WDT1A-PCB WDT1B-PCB WDT1C-PCB WDT1D-PCB            
073000     WDT1E-PCB WDT1F-PCB WDT1G-PCB WDT1H-PCB WDT1I-PCB                    
073100     WDK6-PCB WDD8-PCB WDJ9-PCB SYNQ-ATAB-PCB WDQ3-PCB.                   
073200                                                                          
073300 MAIN SECTION.                                                            
073400     ENTRY 'DLITCBL' USING MSG-PCB ALT1-PCB ALT2-PCB SYNQ-PCB             
073500     USEA-PCB WDT1-PCB WDT1A-PCB WDT1B-PCB WDT1C-PCB WDT1D-PCB            
073600     WDT1E-PCB WDT1F-PCB WDT1G-PCB WDT1H-PCB WDT1I-PCB                    
073700     WDK6-PCB WDD8-PCB WDJ9-PCB SYNQ-ATAB-PCB WDQ3-PCB.                   
073800                                                                          
073900     PERFORM IMS-GET-MSG                                                  
074000     IF SEGMENT-FOUND                                                     
074100       PERFORM A-INIT                                                     
074200       PERFORM B-CHECK-KEYS                                               
074300       IF KEYS-OK                                                         
074400         IF MFS-UPDATE OR W6A164-OK                                       
074500           PERFORM G-CHECK-INPUT                                          
074600           IF INDATA-OK                                                   
074700             PERFORM H-UPDATE                                             
074800           END-IF                                                         
074900           CONTINUE                                                       
075000         ELSE                                                             
075100           IF MFS-FIRST                                                   
075200             PERFORM C-FIRST-PAGE                                         
075300           ELSE                                                           
075400             IF MFS-NEXT                                                  
075500               PERFORM D-NEXT-PAGE                                        
075600             ELSE                                                         
075700               IF MFS-PRINT                                               
075800                  PERFORM S12-CALL-PRINTER                                
075900               ELSE                                                       
076000                  PERFORM E-SAME-PAGE                                     
076100                  PERFORM S11-CHECK-INPUT                                 
076200                  IF JUMP-OK                                              
076300                     PERFORM I-JUMP-6162                                  
076400                  END-IF                                                  
076500               END-IF                                                     
076600             END-IF                                                       
076700           END-IF                                                         
076800         END-IF                                                           
076900         IF ALLT-OK AND W6A164-NOK                                        
077000            PERFORM F-READ-SHOW-INFO                                      
077100         END-IF                                                           
077200       END-IF                                                             
077300*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
077400*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
077500       IF ALLT-OK                                                         
077600         COMPUTE MSG-KVLL = LENGTH OF MOD-W6O16401 + 4                    
077700         PERFORM IMS-INSERT-MSG                                           
077800       END-IF                                                             
077900     END-IF                                                               
078000                                                                          
078100     MOVE ZERO TO RETURN-CODE                                             
078200     GOBACK                                                               
078300     .                                                                    
078400     EJECT                                                                
078500 A-INIT SECTION.                                                          
078600     IF MSG-KDTRANS-1(1:6) = 'W6A164'                                     
078700      MOVE ALL '+' TO MID-W6I16401                                        
078800      MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W6I16402                   
078900      MOVE MSG-IDTRANS-1                TO MFS-IDTRANS                    
079000      MOVE MSG-KDMFSFOR-1               TO MFS-KDMFSFOR                   
079100      MOVE MID-IDORDER-SYNQ(3:9) TO MID-IDARTNR(1)                        
079200      MOVE MID-IDORDER-SYNQ(12:6) TO W-DDDDDD                             
079300      MOVE MID-IDORDER-SYNQ(18:6) TO W-TTTTTT                             
079400      MOVE W-ORDTIME TO MID-TIORDTIME(1)                                  
079500      MOVE 'V' TO MID-KDCMDVAL (1)                                        
079600      MOVE YES TO UPDATE-W6A164                                           
079700     ELSE                                                                 
079800     IF MSG-DOUBLE-TRANSACTIONS                                           
079900       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W6I16401                 
080000       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
080100       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
080200     ELSE                                                                 
080300       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W6I16401                  
080400       MOVE MSG-IDTRANS-1                TO MFS-IDTRANS                   
080500       MOVE MSG-KDMFSFOR-1               TO MFS-KDMFSFOR                  
080600     END-IF                                                               
080700     END-IF                                                               
080800                                                                          
080900     MOVE MSG-KDTRTYP     TO MFS-KDTRTYP                                  
081000     MOVE MSG-IDPFK       TO MFS-IDPFK                                    
081100     MOVE MFS-IDTRANS     TO W-IDTRANS                                    
081200                                                                          
081300     MOVE LOW-VALUE       TO MSG-AREA                                     
081400     MOVE 'W6O164N1'      TO MFS-IDMOD                                    
081500     MOVE '6164'          TO MOD-IDTRANS                                  
081600     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
081700                                                                          
081800     IF OWN-MID OR HELP-MID                                               
081900       CONTINUE                                                           
082000     ELSE                                                                 
082100       MOVE SPACE TO MFS-KDTRTYP                                          
082200       MOVE '7' TO MFS-IDPFK                                              
082300     END-IF                                                               
082400                                                                          
082500     ACCEPT TODAYS-DATE        FROM DATE                                  
082600     ACCEPT TODAYS-TIME        FROM TIME                                  
082700     .                                                                    
082800     EJECT                                                                
082900 B-CHECK-KEYS SECTION.                                                    
083000** CHECK FOR USER DATABASE START **                                       
083100     MOVE ALL '+'           TO MSGI-WMSGINIT                              
083200     MOVE '001'             TO MSGI-KDCALL                                
083300     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
083400     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
083500     MOVE '6164'            TO MSGI-IDTRANS                               
083600     IF OWN-MID OR HELP-MID                                               
083700       IF MID-ADLAGOMR-FOM-IN (2:1) = '*'                                 
083800       AND MID-ADLAGOMR-FOM-IN (1:1) = '0'                                
083900          MOVE MID-ADLAGOMR-FOM-IN (2:1) TO                               
084000               MID-ADLAGOMR-FOM-IN (1:1)                                  
084100          MOVE SPACES TO MID-ADLAGOMR-FOM-IN (2:1)                        
084200       END-IF                                                             
084300                                                                          
084400       IF MID-ADLAGOMR-TOM-IN (2:1) = '*'                                 
084500       AND MID-ADLAGOMR-TOM-IN (1:1) = '0'                                
084600          MOVE MID-ADLAGOMR-TOM-IN (2:1) TO                               
084700               MID-ADLAGOMR-TOM-IN (1:1)                                  
084800          MOVE SPACES TO MID-ADLAGOMR-TOM-IN (2:1)                        
084900       END-IF                                                             
085000                                                                          
085100       IF MID-ADLAGOMR-FOM-IN (1:1) > SPACES                              
085200         IF MID-ADLAGOMR-FOM-IN (1:1) IS NUMERIC                          
085300           IF MID-ADLAGOMR-FOM-IN (2:1) = ' ' OR '+'                      
085400             MOVE MID-ADLAGOMR-FOM-IN (1:1) TO                            
085500                  MID-ADLAGOMR-FOM-IN (2:1)                               
085600             MOVE '0' TO MID-ADLAGOMR-FOM-IN (1:1)                        
085700           END-IF                                                         
085800         END-IF                                                           
085900       END-IF                                                             
086000                                                                          
086100       IF MID-ADGANG-FOM-IN (1:1) > SPACES                                
086200         IF MID-ADGANG-FOM-IN (1:1) IS NUMERIC                            
086300           IF MID-ADGANG-FOM-IN (2:1) = ' ' OR '+'                        
086400              MOVE MID-ADGANG-FOM-IN (1:1) TO                             
086500                   MID-ADGANG-FOM-IN (2:1)                                
086600              MOVE '0' TO MID-ADGANG-FOM-IN (1:1)                         
086700           END-IF                                                         
086800         END-IF                                                           
086900       END-IF                                                             
087000                                                                          
087100       IF MID-ADLAGOMR-TOM-IN (1:1) > SPACES                              
087200         IF MID-ADLAGOMR-TOM-IN (1:1) IS NUMERIC                          
087300           IF MID-ADLAGOMR-TOM-IN (2:1) = ' ' OR '+'                      
087400              MOVE MID-ADLAGOMR-TOM-IN (1:1) TO                           
087500                   MID-ADLAGOMR-TOM-IN (2:1)                              
087600              MOVE '0' TO MID-ADLAGOMR-TOM-IN (1:1)                       
087700           END-IF                                                         
087800         END-IF                                                           
087900       END-IF                                                             
088000                                                                          
088100       IF MID-ADGANG-TOM-IN (1:1) > SPACES                                
088200         IF MID-ADGANG-TOM-IN (1:1) IS NUMERIC                            
088300           IF MID-ADGANG-TOM-IN (2:1) = ' ' OR '+'                        
088400              MOVE MID-ADGANG-TOM-IN (1:1) TO                             
088500                   MID-ADGANG-TOM-IN (2:1)                                
088600              MOVE '0' TO MID-ADGANG-TOM-IN (1:1)                         
088700           END-IF                                                         
088800         END-IF                                                           
088900       END-IF                                                             
089000                                                                          
089100* CONDITION FROM 6160 TO 6164 AND ENTER                                   
089200       IF MID-ADLAGOMR-FOM-IN (2:1) > SPACES                              
089300         IF MID-ADLAGOMR-FOM-IN (2:1) IS NUMERIC                          
089400           IF MID-ADLAGOMR-FOM-IN (1:1) > SPACES                          
089500             CONTINUE                                                     
089600           ELSE                                                           
089700             IF MID-ADLAGOMR-FOM-IN (1:1) = SPACES OR '+'                 
089800               MOVE '0' TO MID-ADLAGOMR-FOM-IN (1:1)                      
089900             END-IF                                                       
090000           END-IF                                                         
090100         ELSE                                                             
090200           IF MID-ADLAGOMR-FOM-IN (2:1) = '*'                             
090300             IF MID-ADLAGOMR-FOM-IN (1:1) = SPACES OR '+'                 
090400               MOVE MID-ADLAGOMR-FOM-IN (2:1) TO                          
090500                    MID-ADLAGOMR-FOM-IN (1:1)                             
090600               MOVE SPACES TO MID-ADLAGOMR-FOM-IN (2:1)                   
090700             ELSE                                                         
090800               CONTINUE                                                   
090900             END-IF                                                       
091000           END-IF                                                         
091100         END-IF                                                           
091200       END-IF                                                             
091300                                                                          
091400       IF MID-ADLAGOMR-TOM-IN (2:1) > SPACES                              
091500         IF MID-ADLAGOMR-TOM-IN (2:1) IS NUMERIC                          
091600           IF MID-ADLAGOMR-TOM-IN (1:1) > SPACES                          
091700              CONTINUE                                                    
091800           ELSE                                                           
091900              IF MID-ADLAGOMR-TOM-IN (1:1) = SPACES OR '+'                
092000                 MOVE '0' TO MID-ADLAGOMR-TOM-IN (1:1)                    
092100              END-IF                                                      
092200           END-IF                                                         
092300         ELSE                                                             
092400           IF MID-ADLAGOMR-TOM-IN (2:1) = '*'                             
092500             IF MID-ADLAGOMR-TOM-IN (1:1) = SPACES OR '+'                 
092600                MOVE MID-ADLAGOMR-TOM-IN (2:1) TO                         
092700                     MID-ADLAGOMR-TOM-IN (1:1)                            
092800                MOVE SPACES TO MID-ADLAGOMR-TOM-IN (2:1)                  
092900             ELSE                                                         
093000                CONTINUE                                                  
093100             END-IF                                                       
093200           END-IF                                                         
093300         END-IF                                                           
093400       END-IF                                                             
093500                                                                          
093600       IF MID-ADLAGOMR-FOM-IN  = ALL '+'                                  
093700       AND MID-ADLAGOMR-TOM-IN = ALL '+'                                  
093800       AND MID-KDSTAPF-IN      = ALL '+'                                  
093900       AND MID-ADGANG-FOM-IN   = ALL '+'                                  
094000       AND MID-ADGANG-TOM-IN   = ALL '+'                                  
094100       AND MID-IDARTNR-IN      = ALL '+'                                  
094200       AND MID-IDUSER-IN       = ALL '+'                                  
094300       AND MID-KDPRIO-PF-IN    = ALL '+'                                  
094400          IF MOD-ADLAGOMR-FOM-UT NOT = ALL '+'                            
094500          OR MOD-ADLAGOMR-TOM-UT NOT = ALL '+'                            
094600          OR MOD-ADGANG-FOM-UT   NOT = ALL '+'                            
094700          OR MOD-ADGANG-TOM-UT   NOT = ALL '+'                            
094800          OR MOD-IDARTNR-UT      NOT = ALL '+'                            
094900          OR MOD-IDUSER-UT       NOT = ALL '+'                            
095000          OR MOD-KDPRIO-PF-UT    NOT = ALL '+'                            
095100            CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                    
095200          ELSE                                                            
095300            MOVE NOO TO KEYS-SW                                           
095400          END-IF                                                          
095500       ELSE                                                               
095600          IF MID-ADLAGOMR-FOM-IN > SPACES AND                             
095700                                   NOT = '++' AND ' *'                    
095800            MOVE MID-ADLAGOMR-FOM-IN TO MSGI-ADLAGOMR-FOM                 
095900          ELSE                                                            
096000            MOVE SPACES TO MSGI-ADLAGOMR-FOM                              
096100          END-IF                                                          
096200                                                                          
096300          IF MID-ADLAGOMR-TOM-IN > SPACES AND                             
096400                                   NOT = '++' AND ' *'                    
096500            MOVE MID-ADLAGOMR-TOM-IN TO MSGI-ADLAGOMR-TOM                 
096600          ELSE                                                            
096700            MOVE SPACES TO MSGI-ADLAGOMR-TOM                              
096800          END-IF                                                          
096900                                                                          
097000          IF MID-KDSTAPF-IN > SPACES AND NOT = '+'                        
097100            MOVE MID-KDSTAPF-IN TO MSGI-KDSTAPF                           
097200          ELSE                                                            
097300            MOVE SPACES TO MSGI-KDSTAPF                                   
097400          END-IF                                                          
097500                                                                          
097600          IF MID-ADGANG-FOM-IN > SPACES AND                               
097700                                   NOT = '++' AND ' *'                    
097800            MOVE MID-ADGANG-FOM-IN TO MSGI-ADGANG-FOM                     
097900          ELSE                                                            
098000            MOVE SPACES TO MSGI-ADGANG-FOM                                
098100          END-IF                                                          
098200                                                                          
098300          IF MID-ADGANG-TOM-IN   > SPACES AND                             
098400                                   NOT = '++' AND ' *'                    
098500            MOVE MID-ADGANG-TOM-IN   TO MSGI-ADGANG-TOM                   
098600          ELSE                                                            
098700            MOVE SPACES TO MSGI-ADGANG-TOM                                
098800          END-IF                                                          
098900                                                                          
099000          IF MID-IDARTNR-IN > SPACES AND NOT = '+++++++++'                
099100            MOVE MID-IDARTNR-IN    TO MSGI-IDARTNR                        
099200          ELSE                                                            
099300            MOVE SPACES            TO MSGI-IDARTNR                        
099400          END-IF                                                          
099500                                                                          
099600          IF MID-IDUSER-IN > SPACES AND NOT = '+++++++'                   
099700            MOVE MID-IDUSER-IN     TO MSGI-IDUSER-KEY                     
099800                                      W-IDUSER                            
099900          ELSE                                                            
100000            MOVE SPACES            TO MSGI-IDUSER-KEY                     
100100                                      W-IDUSER                            
100200          END-IF                                                          
100300          MOVE SAVE-AREA         TO MSGI-SPAR-AREA                        
100400                                                                          
100500          IF MID-KDPRIO-PF-IN > SPACES AND NOT = '+'                      
100600            MOVE MID-KDPRIO-PF-IN    TO MSGI-KDPRIO-PF                    
100700          ELSE                                                            
100800            MOVE SPACES TO MSGI-KDPRIO-PF                                 
100900          END-IF                                                          
101000          CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                      
101100       END-IF                                                             
101200     ELSE                                                                 
101300       MOVE MID-ADLAGOMR-FOM-IN TO MSGI-ADLAGOMR-FOM                      
101400       MOVE MID-ADGANG-FOM-IN   TO MSGI-ADGANG-FOM                        
101500       MOVE MID-ADLAGOMR-TOM-IN TO MSGI-ADLAGOMR-TOM                      
101600       MOVE MID-ADGANG-TOM-IN   TO MSGI-ADGANG-TOM                        
101700       MOVE MID-KDSTAPF-IN      TO MSGI-KDSTAPF                           
101800       MOVE MID-IDARTNR-IN      TO MSGI-IDARTNR                           
101900       MOVE MID-IDUSER-IN       TO MSGI-IDUSER-KEY                        
102000       MOVE MID-KDPRIO-PF-IN    TO MSGI-KDPRIO-PF                         
102100       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
102200     END-IF                                                               
102300                                                                          
102400     MOVE MSGI-ADLAGOMR-FOM TO W-ADLAGOMR-FOM                             
102500     MOVE MSGI-ADGANG-FOM   TO W-ADGANG-FOM                               
102600     MOVE MSGI-ADLAGOMR-TOM TO W-ADLAGOMR-TOM                             
102700     MOVE MSGI-ADGANG-TOM   TO W-ADGANG-TOM                               
102800     MOVE MSGI-KDSTAPF      TO W-KDSTAPF                                  
102900     MOVE MSGI-IDARTNR      TO WS-IDARTNR                                 
103000     INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                       
103100     MOVE 11                TO SYNQ-IDDC                                  
                                     SYNQC-IDDC                                 
103200     MOVE MSGI-IDUSER-KEY   TO W-IDUSER                                   
103300     MOVE MSGI-KDPRIO-PF    TO W-KDPRIO-PF                                
103400     MOVE MSGI-SPAR-AREA    TO SAVE-AREA                                  
103500                                                                          
103600** SETTING VALUES TO MOD FIELDS OF HEADER LINES                           
103700     IF KEYS-OK                                                           
103800       IF MID-ADLAGOMR-FOM-IN = ALL '+'                                   
103900         MOVE W-ADLAGOMR-FOM      TO MOD-ADLAGOMR-FOM-UT                  
104000       ELSE                                                               
104100         MOVE MID-ADLAGOMR-FOM-IN TO MOD-ADLAGOMR-FOM-UT                  
104200                                     W-ADLAGOMR-FOM                       
104300       END-IF                                                             
104400                                                                          
104500       IF MID-ADLAGOMR-TOM-IN = ALL '+'                                   
104600         MOVE W-ADLAGOMR-TOM      TO MOD-ADLAGOMR-TOM-UT                  
104700       ELSE                                                               
104800         MOVE MID-ADLAGOMR-TOM-IN TO MOD-ADLAGOMR-TOM-UT                  
104900                                     W-ADLAGOMR-TOM                       
105000       END-IF                                                             
105100                                                                          
105200       IF MID-KDSTAPF-IN = '+'                                            
105300         MOVE W-KDSTAPF           TO MOD-KDSTAPF-UT                       
105400       ELSE                                                               
105500         MOVE MID-KDSTAPF-IN      TO MOD-KDSTAPF-UT                       
105600       END-IF                                                             
105700                                                                          
105800       IF MID-ADGANG-FOM-IN = ALL '+'                                     
105900         MOVE W-ADGANG-FOM        TO MOD-ADGANG-FOM-UT                    
106000       ELSE                                                               
106100         MOVE MID-ADGANG-FOM-IN   TO MOD-ADGANG-FOM-UT                    
106200                                     W-ADGANG-FOM                         
106300       END-IF                                                             
106400                                                                          
106500       IF MID-ADGANG-TOM-IN = ALL '+'                                     
106600         MOVE W-ADGANG-TOM        TO MOD-ADGANG-TOM-UT                    
106700       ELSE                                                               
106800         MOVE MID-ADGANG-TOM-IN   TO MOD-ADGANG-TOM-UT                    
106900                                     W-ADGANG-TOM                         
107000       END-IF                                                             
107100                                                                          
107200       MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                             
107300                                                                          
107400       IF WS-IDARTNR NUMERIC                                              
107500         IF WS-IDARTNR = ZERO                                             
107600           MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-UT                         
107700         ELSE                                                             
107800           MOVE WS-IDARTNR      TO W-IDARTNR                              
107900         END-IF                                                           
108000       END-IF                                                             
108100                                                                          
108200       IF MID-IDARTNR-IN    =    '+++++++++'                              
108300         MOVE WS-IDARTNR         TO MOD-IDARTNR-UT                        
108400       ELSE                                                               
108500         MOVE MID-IDARTNR-IN     TO MOD-IDARTNR-UT                        
108600       END-IF                                                             
108700       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
108800                                                                          
108900       IF MID-IDUSER-IN     = ALL '+'                                     
109000         MOVE W-IDUSER          TO MOD-IDUSER-UT                          
109100       ELSE                                                               
109200         MOVE MID-IDUSER-IN     TO MOD-IDUSER-UT                          
109300                                   W-IDUSER                               
109400       END-IF                                                             
109500                                                                          
109600       IF MID-KDPRIO-PF-IN  = ALL '+'                                     
109700         MOVE W-KDPRIO-PF       TO MOD-KDPRIO-PF-UT                       
109800       ELSE                                                               
109900         MOVE MID-KDPRIO-PF-IN  TO MOD-KDPRIO-PF-UT                       
110000                                   W-KDPRIO-PF                            
110100       END-IF                                                             
110200     ELSE                                                                 
110300       MOVE MID-ADLAGOMR-FOM-IN TO MOD-ADLAGOMR-FOM-UT                    
110400       MOVE MID-ADLAGOMR-TOM-IN TO MOD-ADLAGOMR-TOM-UT                    
110500       MOVE MID-KDSTAPF-IN      TO MOD-KDSTAPF-UT                         
110600       MOVE MID-ADGANG-FOM-IN   TO MOD-ADGANG-FOM-UT                      
110700       MOVE MID-ADGANG-TOM-IN   TO MOD-ADGANG-TOM-UT                      
110800       MOVE MID-IDARTNR-IN      TO MOD-IDARTNR-UT                         
110900       MOVE MID-IDUSER-IN       TO MOD-IDUSER-UT                          
111000       MOVE MID-KDPRIO-PF-IN    TO MOD-KDPRIO-PF-UT                       
111100     END-IF                                                               
111200                                                                          
111300** VALIDATION OF KDSTAPF FIELD                                            
111400     IF (W-KDSTAPF NOT = 'R' AND 'L' AND 'V' AND '+' AND ' '              
111500                         AND 'E')                                         
111600       MOVE NOO  TO KEYS-SW                                               
111700     ELSE                                                                 
111800       IF W-ADLAGOMR-FOM = SPACES AND W-ADLAGOMR-TOM = SPACES             
111900         AND W-IDARTNR = 0 AND W-IDUSER = SPACES                          
112000         AND W-KDPRIO-PF = SPACES AND W6A164-NOK                          
112100         MOVE NOO  TO KEYS-SW                                             
112200       END-IF                                                             
112300     END-IF                                                               
112400                                                                          
112500** VALIDATION OF KDPRIO-PF FIELD                                          
112600     IF (W-KDPRIO-PF  NOT = 'Y' AND 'J' AND 'N' AND '+' AND ' ')          
112700       AND W6A164-NOK                                                     
112800       MOVE NOO  TO KEYS-SW                                               
112900     END-IF                                                               
113000                                                                          
113100     IF W-KDPRIO-PF = 'J' OR 'Y'                                          
113200       MOVE '1'             TO W-KDPRIO-PF                                
113300     ELSE                                                                 
113400       IF W-KDPRIO-PF = 'N'                                               
113500         MOVE '2'           TO W-KDPRIO-PF                                
113600       END-IF                                                             
113700     END-IF                                                               
113800                                                                          
113900** VALIDATION OF MID AREA FOR KEY FIELDS                                  
114000     IF (W-ADLAGOMR-FOM > SPACES AND W-ADGANG-TOM   > SPACES)  OR         
114100        (W-ADLAGOMR-TOM > SPACES AND W-ADGANG-FOM   > SPACES)  OR         
114200                                                                          
114300        (W-ADLAGOMR-FOM IS NUMERIC AND W-ADGANG-FOM > SPACES              
114400         AND W-ADGANG-FOM IS NOT NUMERIC)                      OR         
114500                                                                          
114600        (W-ADLAGOMR-TOM IS NUMERIC AND W-ADGANG-TOM > SPACES              
114700         AND W-ADGANG-TOM IS NOT NUMERIC)                      OR         
114800                                                                          
114900        (W-ADGANG-FOM IS NUMERIC AND                                      
115000         W-ADLAGOMR-FOM IS NOT NUMERIC)                        OR         
115100        (W-ADGANG-FOM IS NUMERIC AND W-ADGANG-TOM IS NUMERIC)  OR         
115200        (W-ADGANG-FOM IS NUMERIC AND W-IDARTNR      > 0     )  OR         
115300        (W-ADGANG-FOM IS NUMERIC AND W-IDUSER       > SPACES)  OR         
115400        (W-ADGANG-FOM IS NUMERIC AND W-KDPRIO-PF    > SPACES)  OR         
115500                                                                          
115600        (W-ADGANG-TOM IS NUMERIC AND                                      
115700         W-ADLAGOMR-TOM IS NOT NUMERIC)                        OR         
115800        (W-ADGANG-TOM IS NUMERIC AND W-IDARTNR      > 0     )  OR         
115900        (W-ADGANG-TOM IS NUMERIC AND W-IDUSER       > SPACES)  OR         
116000        (W-ADGANG-TOM IS NUMERIC AND W-KDPRIO-PF    > SPACES)  OR         
116100                                                                          
116200        (W-IDARTNR > 0      AND W-IDUSER    > SPACES)          OR         
116300        (W-IDARTNR > 0      AND W-KDPRIO-PF > SPACES)          OR         
116400                                                                          
116500        (W-IDUSER  > SPACES AND W-KDPRIO-PF > SPACES)                     
116600                                                                          
116700                                                                          
116800       MOVE NOO  TO KEYS-SW                                               
116900     ELSE                                                                 
117000       IF (W-ADLAGOMR-FOM > SPACES)                                       
117100         IF W-ADLAGOMR-FOM NOT NUMERIC                                    
117200           IF (W-ADLAGOMR-FOM = '* ') AND                                 
117300              (W-KDSTAPF = 'R' OR 'L' OR                                  
117400                          'V' OR ' ' OR '+' OR 'E')                       
117500              CONTINUE                                                    
117600           ELSE                                                           
117700              MOVE NOO  TO KEYS-SW                                        
117800           END-IF                                                         
117900         ELSE                                                             
118000           IF (W-KDSTAPF = 'R' OR 'L' OR                                  
118100                          'V' OR ' ' OR '+' OR 'E')                       
118200              CONTINUE                                                    
118300           ELSE                                                           
118400              MOVE NOO  TO KEYS-SW                                        
118500           END-IF                                                         
118600         END-IF                                                           
118700       ELSE                                                               
118800         IF (W-ADLAGOMR-TOM > SPACES)                                     
118900           IF W-ADLAGOMR-TOM NOT NUMERIC                                  
119000             IF (W-ADLAGOMR-TOM = '* ') AND                               
119100                (W-KDSTAPF = 'R' OR 'L' OR 'V' OR ' ' OR '+'              
119200                             OR 'E')                                      
119300               CONTINUE                                                   
119400             ELSE                                                         
119500               MOVE NOO  TO KEYS-SW                                       
119600             END-IF                                                       
119700           ELSE                                                           
119800             IF (W-KDSTAPF = 'R' OR 'L' OR 'V' OR ' ' OR '+'              
119900                             OR 'E')                                      
120000               CONTINUE                                                   
120100             ELSE                                                         
120200               MOVE NOO  TO KEYS-SW                                       
120300             END-IF                                                       
120400           END-IF                                                         
120500         ELSE                                                             
120600           CONTINUE                                                       
120700         END-IF                                                           
120800       END-IF                                                             
120900     END-IF                                                               
121000                                                                          
121100** CALLING ERROR MESSAGE FOR INVALID KEYS                                 
121200     IF KEYS-WRONG                                                        
121300       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
121400       CALL WMEDKONV USING MED-WMEDAREA                                   
121500       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
121600       PERFORM MFS-ERASE-FIELD-IN                                         
121700       IF NOT OWN-MID OR NOT GOOD-MID                                     
121800         PERFORM MFS-ERASE-FIELD-OUT                                      
121900       END-IF                                                             
122000     END-IF                                                               
122100                                                                          
122200** SETTING MIN AND MAX VALUES FOR THE KEYS IN WDT1 DATABASE               
122300     IF KEYS-OK                                                           
122400**     START OF WDT1A                                                     
122500       IF W-ADLAGOMR-FOM = '* ' AND W-ADGANG-FOM = SPACES                 
122600         MOVE ZERO TO W-ADLAGFOM-WDT1A-MIN                                
122700         MOVE 'WDT1A' TO WDT1-CHECK-SW                                    
122800       ELSE                                                               
122900         IF W-ADLAGOMR-FOM NOT = '++' AND '  ' AND '* '                   
123000           AND W-ADGANG-FOM = SPACES                                      
123100           IF W-ADLAGOMR-FOM(2:1) = ' '                                   
123200             MOVE W-ADLAGOMR-FOM(1:1) TO W-ADLAGOMR-FOM(2:1)              
123300             MOVE '0' TO W-ADLAGOMR-FOM(1:1)                              
123400           END-IF                                                         
123500           MOVE W-ADLAGOMR-FOM TO W-ADLAGFOM-WDT1A-MIN                    
123600                                  W-ADLAGFOM-WDT1A-MAX                    
123700           MOVE 'WDT1A' TO WDT1-CHECK-SW                                  
123800         END-IF                                                           
123900       END-IF                                                             
124000                                                                          
124100       IF WDT1A1-CHECK                                                    
124200         IF (W-KDSTAPF = ' ' OR '+')                                      
124300           MOVE LOW-VALUE  TO W-KDSTAPF-WDT1A-MIN                         
124400           MOVE HIGH-VALUE TO W-KDSTAPF-WDT1A-MAX                         
124500         ELSE                                                             
124600           MOVE W-KDSTAPF TO W-KDSTAPF-WDT1A-MIN                          
124700                             W-KDSTAPF-WDT1A-MAX                          
124800         END-IF                                                           
124900       END-IF                                                             
125000                                                                          
125100**     START OF WDT1B                                                     
125200       IF (W-ADLAGOMR-TOM = '* ' OR '+ ') AND                             
125210           W-ADGANG-TOM = SPACES                                          
125300         MOVE ZERO TO W-ADLAGTOM-WDT1B-MIN                                
125400         MOVE 'WDT1B' TO WDT1-CHECK-SW                                    
125500       ELSE                                                               
125600         IF W-ADLAGOMR-TOM NOT = '++' AND '  ' AND '* '                   
125700         AND W-ADGANG-TOM = SPACES                                        
125800            IF W-ADLAGOMR-TOM(2:1) = ' '                                  
125900              MOVE W-ADLAGOMR-TOM(1:1) TO W-ADLAGOMR-TOM(2:1)             
126000              MOVE '0' TO W-ADLAGOMR-TOM(1:1)                             
126100            END-IF                                                        
126200            MOVE W-ADLAGOMR-TOM TO W-ADLAGTOM-WDT1B-MIN                   
126300                                   W-ADLAGTOM-WDT1B-MAX                   
126400            MOVE 'WDT1B' TO WDT1-CHECK-SW                                 
126500         END-IF                                                           
126600       END-IF                                                             
126700                                                                          
126800       IF WDT1B1-CHECK                                                    
126900         IF (W-KDSTAPF = ' ' OR '+')                                      
127000           MOVE LOW-VALUE TO  W-KDSTAPF-WDT1B-MIN                         
127100           MOVE HIGH-VALUE TO W-KDSTAPF-WDT1B-MAX                         
127200         ELSE                                                             
127300           MOVE W-KDSTAPF TO W-KDSTAPF-WDT1B-MIN                          
127400                             W-KDSTAPF-WDT1B-MAX                          
127500         END-IF                                                           
127600       END-IF                                                             
127700                                                                          
127800**     START OF WDT1I                                                     
127900       IF W-ADLAGOMR-FOM IS NUMERIC AND                                   
128000          W-ADLAGOMR-TOM IS NUMERIC                                       
128100         MOVE W-ADLAGOMR-FOM TO W-ADLAGFOM-WDT1I-MIN                      
128200                                W-ADLAGFOM-WDT1I-MAX                      
128300         MOVE W-ADLAGOMR-TOM TO W-ADLAGTOM-WDT1I-MIN                      
128400                                W-ADLAGTOM-WDT1I-MAX                      
128500         MOVE 'WDT1I' TO WDT1-CHECK-SW                                    
128600       END-IF                                                             
128700                                                                          
128800       IF WDT1I1-CHECK                                                    
128900         IF (W-KDSTAPF = ' ' OR '+')                                      
129000           MOVE LOW-VALUE TO  W-KDSTAPF-WDT1I-MIN                         
129100           MOVE HIGH-VALUE TO W-KDSTAPF-WDT1I-MAX                         
129200         ELSE                                                             
129300           MOVE W-KDSTAPF TO W-KDSTAPF-WDT1I-MIN                          
129400                             W-KDSTAPF-WDT1I-MAX                          
129500         END-IF                                                           
129600       END-IF                                                             
129700                                                                          
129800**     START OF WDT1C                                                     
129900       IF W-ADLAGOMR-FOM IS NUMERIC AND W-ADGANG-FOM IS NUMERIC           
130000         MOVE 'WDT1C' TO WDT1-CHECK-SW                                    
130100         MOVE W-ADLAGOMR-FOM TO W-ADLAGFOM-WDT1C-MIN                      
130200                                W-ADLAGFOM-WDT1C-MAX                      
130300         MOVE W-ADGANG-FOM   TO W-ADGANGFOM-WDT1C-MIN                     
130400                                W-ADGANGFOM-WDT1C-MAX                     
130500         MOVE 'WDT1C' TO WDT1-CHECK-SW                                    
130600       END-IF                                                             
130700                                                                          
130800**     START OF WDT1D                                                     
130900       IF W-ADLAGOMR-TOM IS NUMERIC AND W-ADGANG-TOM IS NUMERIC           
131000         MOVE 'WDT1D' TO WDT1-CHECK-SW                                    
131100         MOVE W-ADLAGOMR-TOM TO W-ADLAGTOM-WDT1D-MIN                      
131200                                W-ADLAGTOM-WDT1D-MAX                      
131300         MOVE W-ADGANG-TOM   TO W-ADGANGTOM-WDT1D-MIN                     
131400                                W-ADGANGTOM-WDT1D-MAX                     
131500         MOVE 'WDT1D' TO WDT1-CHECK-SW                                    
131600       END-IF                                                             
131700                                                                          
131800**     START OF WDT10                                                     
131900       IF W-IDARTNR IS NUMERIC AND W-IDARTNR NOT = 0                      
132000         MOVE 'WDT10' TO WDT1-CHECK-SW                                    
132100         MOVE W-IDARTNR      TO W-IDARTNR-WDT101-MIN                      
132200                                W-IDARTNR-WDT101-MAX                      
132300         IF W-ADLAGOMR-FOM IS NUMERIC                                     
132400           MOVE W-ADLAGOMR-FOM TO W-ADLAGOMR-FOM-N                        
132500         ELSE                                                             
132600           IF W-ADLAGOMR-TOM IS NUMERIC                                   
132700             MOVE W-ADLAGOMR-TOM TO W-ADLAGOMR-TOM-N                      
132800           END-IF                                                         
132900         END-IF                                                           
133000       END-IF                                                             
133100                                                                          
133200**     START OF WDT1E                                                     
133300       IF W-IDUSER > SPACES                                               
133400         MOVE 'WDT1E' TO WDT1-CHECK-SW                                    
133500         MOVE W-IDUSER     TO W-IDUSER-WDT1E-MIN                          
133600                              W-IDUSER-WDT1F-MIN                          
133700                              W-IDUSER-WDT1E-MAX                          
133800                              W-IDUSER-WDT1F-MAX                          
133900         IF W-KDSTAPF = ' ' OR '+'                                        
134000           MOVE LOW-VALUE  TO W-KDSTAPF-WDT1E-MIN                         
134100                              W-KDSTAPF-WDT1F-MIN                         
134200           MOVE HIGH-VALUE TO W-KDSTAPF-WDT1E-MAX                         
134300                              W-KDSTAPF-WDT1F-MAX                         
134400         ELSE                                                             
134500           MOVE W-KDSTAPF  TO W-KDSTAPF-WDT1E-MIN                         
134600                              W-KDSTAPF-WDT1F-MIN                         
134700                              W-KDSTAPF-WDT1E-MAX                         
134800                              W-KDSTAPF-WDT1F-MAX                         
134900         END-IF                                                           
135000                                                                          
135100         IF W-ADLAGOMR-FOM IS NUMERIC                                     
135200           MOVE W-ADLAGOMR-FOM TO W-ADLAGFOM-WDT1E-MIN                    
135300                                  W-ADLAGFOM-WDT1E-MAX                    
135400         ELSE                                                             
135500**         START OF WDT1F                                                 
135600           IF W-ADLAGOMR-TOM IS NUMERIC                                   
135700             MOVE 'WDT1F' TO WDT1-CHECK-SW                                
135800             MOVE W-ADLAGOMR-TOM TO W-ADLAGTOM-WDT1F-MIN                  
135900                                    W-ADLAGTOM-WDT1F-MAX                  
136000           END-IF                                                         
136100         END-IF                                                           
136200       END-IF                                                             
136300                                                                          
136400**     START OF WDT1G/H                                                   
136500       IF W-KDPRIO-PF IS NUMERIC                                          
136600         MOVE 'WDT1G' TO WDT1-CHECK-SW                                    
136700         MOVE W-KDPRIO-PF  TO W-KDPRIO-WDT1G-MIN                          
136800                              W-KDPRIO-WDT1H-MIN                          
136900                              W-KDPRIO-WDT1G-MAX                          
137000                              W-KDPRIO-WDT1H-MAX                          
137100         IF W-KDSTAPF = ' ' OR '+'                                        
137200           MOVE LOW-VALUE  TO W-KDSTAPF-WDT1G-MIN                         
137300                              W-KDSTAPF-WDT1H-MIN                         
137400           MOVE HIGH-VALUE TO W-KDSTAPF-WDT1G-MAX                         
137500                              W-KDSTAPF-WDT1H-MAX                         
137600         ELSE                                                             
137700           MOVE W-KDSTAPF  TO W-KDSTAPF-WDT1G-MIN                         
137800                              W-KDSTAPF-WDT1H-MIN                         
137900                              W-KDSTAPF-WDT1G-MAX                         
138000                              W-KDSTAPF-WDT1H-MAX                         
138100         END-IF                                                           
138200                                                                          
138300         IF W-ADLAGOMR-FOM IS NUMERIC                                     
138400           MOVE W-ADLAGOMR-FOM TO W-ADLAGFOM-WDT1G-MIN                    
138500                                  W-ADLAGFOM-WDT1G-MAX                    
138600         ELSE                                                             
138700           IF W-ADLAGOMR-TOM IS NUMERIC                                   
138800             MOVE 'WDT1H' TO WDT1-CHECK-SW                                
138900             MOVE W-ADLAGOMR-TOM TO W-ADLAGTOM-WDT1H-MIN                  
139000                                    W-ADLAGTOM-WDT1H-MAX                  
139100           END-IF                                                         
139200         END-IF                                                           
139300       END-IF                                                             
139400                                                                          
139500     END-IF                                                               
139600     .                                                                    
139700     EJECT                                                                
139800 C-FIRST-PAGE SECTION.                                                    
139900     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
140000     CALL WMEDKONV USING MED-WMEDAREA                                     
140100     MOVE MED-MFSINF     TO MOD-TEMFSFEL                                  
140200                                                                          
140300     PERFORM MFS-ERASE-FIELD-IN                                           
140400     .                                                                    
140500     EJECT                                                                
140600 D-NEXT-PAGE SECTION.                                                     
140700     IF SAVE-IDTRANS = '6164'                                             
140800       IF MID-ADLAGOMR-FOM-IN = '++' AND                                  
140900          MID-ADGANG-FOM-IN   = '++' AND                                  
141000          MID-ADLAGOMR-TOM-IN = '++' AND                                  
141100          MID-ADGANG-TOM-IN   = '++' AND                                  
141200          MID-KDSTAPF-IN      = '+'  AND                                  
141300          MID-IDARTNR-IN      = '+++++++++' AND                           
141400          MID-IDUSER-IN       = '+++++++'   AND                           
141500          MID-KDPRIO-PF-IN = '+'                                          
141600         EVALUATE TRUE                                                    
141700         WHEN WDT1A1-CHECK                                                
141800             MOVE SAVE-IDDC-NEXT         TO W-IDDC-WDT1A-MIN              
141900             MOVE SAVE-ADLAGOMR-ADR-NEXT TO W-ADLAGFOM-WDT1A-MIN          
142000             MOVE SAVE-KDSTAPF-NEXT      TO W-KDSTAPF-WDT1A-MIN           
142100             MOVE SAVE-KDPRIO-PF-NEXT    TO W-KDPRIO-PF-WDT1A-MIN         
142200             MOVE SAVE-TIORDTIME-NEXT    TO W-TIORDTIME-WDT1A-MIN         
142300         WHEN WDT1B1-CHECK                                                
142400             MOVE SAVE-IDDC-NEXT         TO W-IDDC-WDT1B-MIN              
142500             MOVE SAVE-ADLAGOMR-ADR-NEXT TO W-ADLAGTOM-WDT1B-MIN          
142600             MOVE SAVE-KDSTAPF-NEXT      TO W-KDSTAPF-WDT1B-MIN           
142700             MOVE SAVE-KDPRIO-PF-NEXT    TO W-KDPRIO-PF-WDT1B-MIN         
142800             MOVE SAVE-TIORDTIME-NEXT    TO W-TIORDTIME-WDT1B-MIN         
142900         WHEN WDT1C1-CHECK                                                
143000             MOVE SAVE-IDDC-WDT1C-N      TO W-IDDC-WDT1C-MIN              
143100             MOVE SAVE-ADLAGFOM-WDT1C-N  TO W-ADLAGFOM-WDT1C-MIN          
143200             MOVE SAVE-ADGANGFOM-WDT1C-N TO W-ADGANGFOM-WDT1C-MIN         
143300             MOVE SAVE-ADPLATFOM-WDT1C-N TO W-ADPLATFOM-WDT1C-MIN         
143400             MOVE SAVE-TIORDTIME-WDT1C-N TO W-TIORDTIME-WDT1C-MIN         
143500         WHEN WDT1D1-CHECK                                                
143600             MOVE SAVE-IDDC-WDT1D-N      TO W-IDDC-WDT1D-MIN              
143700             MOVE SAVE-ADLAGTOM-WDT1D-N  TO W-ADLAGTOM-WDT1D-MIN          
143800             MOVE SAVE-ADGANGTOM-WDT1D-N TO W-ADGANGTOM-WDT1D-MIN         
143900             MOVE SAVE-ADPLATTOM-WDT1D-N TO W-ADPLATTOM-WDT1D-MIN         
144000             MOVE SAVE-TIORDTIME-WDT1D-N TO W-TIORDTIME-WDT1D-MIN         
144100         WHEN WDT1E1-CHECK                                                
144200             MOVE SAVE-IDDC-WDT1E-N      TO W-IDDC-WDT1E-MIN              
144300             MOVE SAVE-IDUSER-WDT1E-N    TO W-IDUSER-WDT1E-MIN            
144400             MOVE SAVE-ADLAGFOM-WDT1E-N  TO W-ADLAGFOM-WDT1E-MIN          
144500             MOVE SAVE-ADGANGFOM-WDT1E-N TO W-ADGANGFOM-WDT1E-MIN         
144600             MOVE SAVE-ADPLATFOM-WDT1E-N TO W-ADPLATFOM-WDT1E-MIN         
144700             MOVE SAVE-KDSTAPF-WDT1E-N   TO W-KDSTAPF-WDT1E-MIN           
144800             MOVE SAVE-TIORDTIME-WDT1E-N TO W-TIORDTIME-WDT1E-MIN         
144900         WHEN WDT1F1-CHECK                                                
145000             MOVE SAVE-IDDC-WDT1F-N      TO W-IDDC-WDT1F-MIN              
145100             MOVE SAVE-IDUSER-WDT1F-N    TO W-IDUSER-WDT1F-MIN            
145200             MOVE SAVE-ADLAGTOM-WDT1F-N  TO W-ADLAGTOM-WDT1F-MIN          
145300             MOVE SAVE-ADGANGTOM-WDT1F-N TO W-ADGANGTOM-WDT1F-MIN         
145400             MOVE SAVE-ADPLATTOM-WDT1F-N TO W-ADPLATTOM-WDT1F-MIN         
145500             MOVE SAVE-KDSTAPF-WDT1F-N   TO W-KDSTAPF-WDT1F-MIN           
145600             MOVE SAVE-TIORDTIME-WDT1F-N TO W-TIORDTIME-WDT1F-MIN         
145700         WHEN WDT1G1-CHECK                                                
145800             MOVE SAVE-IDDC-WDT1G-N      TO W-IDDC-WDT1G-MIN              
145900             MOVE SAVE-KDPRIO-WDT1G-N    TO W-KDPRIO-WDT1G-MIN            
146000             MOVE SAVE-ADLAGFOM-WDT1G-N  TO W-ADLAGFOM-WDT1G-MIN          
146100             MOVE SAVE-ADGANGFOM-WDT1G-N TO W-ADGANGFOM-WDT1G-MIN         
146200             MOVE SAVE-ADPLATFOM-WDT1G-N TO W-ADPLATFOM-WDT1G-MIN         
146300             MOVE SAVE-KDSTAPF-WDT1G-N   TO W-KDSTAPF-WDT1G-MIN           
146400             MOVE SAVE-TIORDTIME-WDT1G-N TO W-TIORDTIME-WDT1G-MIN         
146500         WHEN WDT1H1-CHECK                                                
146600             MOVE SAVE-IDDC-WDT1H-N      TO W-IDDC-WDT1H-MIN              
146700             MOVE SAVE-KDPRIO-WDT1H-N    TO W-KDPRIO-WDT1H-MIN            
146800             MOVE SAVE-ADLAGTOM-WDT1H-N  TO W-ADLAGTOM-WDT1H-MIN          
146900             MOVE SAVE-ADGANGTOM-WDT1H-N TO W-ADGANGTOM-WDT1H-MIN         
147000             MOVE SAVE-ADPLATTOM-WDT1H-N TO W-ADPLATTOM-WDT1H-MIN         
147100             MOVE SAVE-KDSTAPF-WDT1H-N   TO W-KDSTAPF-WDT1H-MIN           
147200             MOVE SAVE-TIORDTIME-WDT1H-N TO W-TIORDTIME-WDT1H-MIN         
147300         WHEN WDT1I1-CHECK                                                
147400             MOVE SAVE-IDDC-WDT1I-N      TO W-IDDC-WDT1I-MIN              
147500             MOVE SAVE-ADLAGFOM-WDT1I-N  TO W-ADLAGFOM-WDT1I-MIN          
147600             MOVE SAVE-ADLAGTOM-WDT1I-N  TO W-ADLAGTOM-WDT1I-MIN          
147700             MOVE SAVE-KDPRIO-WDT1I-N    TO W-KDPRIO-WDT1I-MIN            
147800             MOVE SAVE-ADGANGFOM-WDT1I-N TO W-ADGANGFOM-WDT1I-MIN         
147900             MOVE SAVE-ADPLATFOM-WDT1I-N TO W-ADPLATFOM-WDT1I-MIN         
148000             MOVE SAVE-KDSTAPF-WDT1I-N   TO W-KDSTAPF-WDT1I-MIN           
148100             MOVE SAVE-TIORDTIME-WDT1I-N TO W-TIORDTIME-WDT1I-MIN         
148200          WHEN WDT101-CHECK                                               
148300            MOVE SAVE-IDDC-WDT101-N      TO W-IDDC-WDT101-MIN             
148400            MOVE SAVE-IDARTNR-WDT101-N   TO W-IDARTNR-WDT101-MIN          
148500            MOVE SAVE-TIORDTIME-WDT101-N TO W-TIORDTIME-WDT101-MIN        
148600         END-EVALUATE                                                     
148700       ELSE                                                               
148800         MOVE INF-FIRST-PAGE TO MED-IDMFSINF                              
148900         CALL WMEDKONV USING MED-WMEDAREA                                 
149000         MOVE MED-MFSINF     TO MOD-TEMFSFEL                              
149100       END-IF                                                             
149200     END-IF                                                               
149300     PERFORM MFS-ERASE-FIELD-IN                                           
149400     .                                                                    
149500     EJECT                                                                
149600 E-SAME-PAGE SECTION.                                                     
149700     MOVE 0  TO W-CMD-C                                                   
149800     MOVE +1 TO INDX                                                      
149900     PERFORM UNTIL INDX > MAX-INDX  OR W-CMD-C > 0                        
150000      IF MID-KDCMDVAL (INDX) = 'C'                                        
150100        ADD 1  TO W-CMD-C                                                 
150200        MOVE INDX TO W-INDX                                               
150300        MOVE MFS-ADD-SET-CURSOR   TO MOD-ADLAGOMR-FOM-N-ATTR              
150400        MOVE MID-KDCMDVAL (INDX)  TO MOD-KDCMDVAL (INDX)                  
150500      END-IF                                                              
150600      ADD +1 TO INDX                                                      
150700     END-PERFORM                                                          
150800                                                                          
150900     IF SAVE-IDTRANS = '6164' OR '0551'                                   
151000       IF MID-ADLAGOMR-FOM-IN = '++' AND                                  
151100          MID-ADGANG-FOM-IN   = '++' AND                                  
151200          MID-ADLAGOMR-TOM-IN = '++' AND                                  
151300          MID-ADGANG-TOM-IN   = '++' AND                                  
151400          MID-KDSTAPF-IN      = '+'  AND                                  
151500          MID-IDARTNR-IN      = '+++++++++' AND                           
151600          MID-IDUSER-IN       = '+++++++'   AND                           
151700          MID-KDPRIO-PF-IN = '+'                                          
151800          EVALUATE TRUE                                                   
151900          WHEN WDT1A1-CHECK                                               
152000            MOVE SAVE-IDDC-ENTER        TO W-IDDC-WDT1A-MIN               
152100            MOVE SAVE-ADLAGOMR-ADR-ENTER TO W-ADLAGFOM-WDT1A-MIN          
152200            MOVE SAVE-KDSTAPF-ENTER     TO W-KDSTAPF-WDT1A-MIN            
152300            MOVE SAVE-KDPRIO-PF-ENTER   TO W-KDPRIO-PF-WDT1A-MIN          
152400            MOVE SAVE-TIORDTIME-ENTER   TO W-TIORDTIME-WDT1A-MIN          
152500          WHEN WDT1B1-CHECK                                               
152600            MOVE SAVE-IDDC-ENTER        TO W-IDDC-WDT1B-MIN               
152700            MOVE SAVE-ADLAGOMR-ADR-ENTER TO W-ADLAGTOM-WDT1B-MIN          
152800            MOVE SAVE-KDSTAPF-ENTER     TO W-KDSTAPF-WDT1B-MIN            
152900            MOVE SAVE-KDPRIO-PF-ENTER   TO W-KDPRIO-PF-WDT1B-MIN          
153000            MOVE SAVE-TIORDTIME-ENTER   TO W-TIORDTIME-WDT1B-MIN          
153100          WHEN WDT1C1-CHECK                                               
153200            MOVE SAVE-IDDC-WDT1C-E      TO W-IDDC-WDT1C-MIN               
153300            MOVE SAVE-ADLAGFOM-WDT1C-E  TO W-ADLAGFOM-WDT1C-MIN           
153400            MOVE SAVE-ADGANGFOM-WDT1C-E TO W-ADGANGFOM-WDT1C-MIN          
153500            MOVE SAVE-ADPLATFOM-WDT1C-E TO W-ADPLATFOM-WDT1C-MIN          
153600            MOVE SAVE-TIORDTIME-WDT1C-E TO W-TIORDTIME-WDT1C-MIN          
153700          WHEN WDT1D1-CHECK                                               
153800            MOVE SAVE-IDDC-WDT1D-E      TO W-IDDC-WDT1D-MIN               
153900            MOVE SAVE-ADLAGTOM-WDT1D-E  TO W-ADLAGTOM-WDT1D-MIN           
154000            MOVE SAVE-ADGANGTOM-WDT1D-E TO W-ADGANGTOM-WDT1D-MIN          
154100            MOVE SAVE-ADPLATTOM-WDT1D-E TO W-ADPLATTOM-WDT1D-MIN          
154200            MOVE SAVE-TIORDTIME-WDT1D-E TO W-TIORDTIME-WDT1D-MIN          
154300          WHEN WDT1E1-CHECK                                               
154400            MOVE SAVE-IDDC-WDT1E-E      TO W-IDDC-WDT1E-MIN               
154500            MOVE SAVE-IDUSER-WDT1E-E    TO W-IDUSER-WDT1E-MIN             
154600            MOVE SAVE-ADLAGFOM-WDT1E-E  TO W-ADLAGFOM-WDT1E-MIN           
154700            MOVE SAVE-ADGANGFOM-WDT1E-E TO W-ADGANGFOM-WDT1E-MIN          
154800            MOVE SAVE-ADPLATFOM-WDT1E-E TO W-ADPLATFOM-WDT1E-MIN          
154900            MOVE SAVE-KDSTAPF-WDT1E-E   TO W-KDSTAPF-WDT1E-MIN            
155000            MOVE SAVE-TIORDTIME-WDT1E-E TO W-TIORDTIME-WDT1E-MIN          
155100          WHEN WDT1F1-CHECK                                               
155200            MOVE SAVE-IDDC-WDT1F-E      TO W-IDDC-WDT1F-MIN               
155300            MOVE SAVE-IDUSER-WDT1F-E    TO W-IDUSER-WDT1F-MIN             
155400            MOVE SAVE-ADLAGTOM-WDT1F-E  TO W-ADLAGTOM-WDT1F-MIN           
155500            MOVE SAVE-ADGANGTOM-WDT1F-E TO W-ADGANGTOM-WDT1F-MIN          
155600            MOVE SAVE-ADPLATTOM-WDT1F-E TO W-ADPLATTOM-WDT1F-MIN          
155700            MOVE SAVE-KDSTAPF-WDT1F-E   TO W-KDSTAPF-WDT1F-MIN            
155800            MOVE SAVE-TIORDTIME-WDT1F-E TO W-TIORDTIME-WDT1F-MIN          
155900          WHEN WDT1G1-CHECK                                               
156000            MOVE SAVE-IDDC-WDT1G-E      TO W-IDDC-WDT1G-MIN               
156100            MOVE SAVE-KDPRIO-WDT1G-E    TO W-KDPRIO-WDT1G-MIN             
156200            MOVE SAVE-ADLAGFOM-WDT1G-E  TO W-ADLAGFOM-WDT1G-MIN           
156300            MOVE SAVE-ADGANGFOM-WDT1G-E TO W-ADGANGFOM-WDT1G-MIN          
156400            MOVE SAVE-ADPLATFOM-WDT1G-E TO W-ADPLATFOM-WDT1G-MIN          
156500            MOVE SAVE-KDSTAPF-WDT1G-E   TO W-KDSTAPF-WDT1G-MIN            
156600            MOVE SAVE-TIORDTIME-WDT1G-E TO W-TIORDTIME-WDT1G-MIN          
156700          WHEN WDT1H1-CHECK                                               
156800            MOVE SAVE-IDDC-WDT1H-E      TO W-IDDC-WDT1H-MIN               
156900            MOVE SAVE-KDPRIO-WDT1H-E    TO W-KDPRIO-WDT1H-MIN             
157000            MOVE SAVE-ADLAGTOM-WDT1H-E  TO W-ADLAGTOM-WDT1H-MIN           
157100            MOVE SAVE-ADGANGTOM-WDT1H-E TO W-ADGANGTOM-WDT1H-MIN          
157200            MOVE SAVE-ADPLATTOM-WDT1H-E TO W-ADPLATTOM-WDT1H-MIN          
157300            MOVE SAVE-KDSTAPF-WDT1H-E   TO W-KDSTAPF-WDT1H-MIN            
157400            MOVE SAVE-TIORDTIME-WDT1H-E TO W-TIORDTIME-WDT1H-MIN          
157500          WHEN WDT1I1-CHECK                                               
157600            MOVE SAVE-IDDC-WDT1I-E      TO W-IDDC-WDT1I-MIN               
157700            MOVE SAVE-ADLAGFOM-WDT1I-E  TO W-ADLAGFOM-WDT1I-MIN           
157800            MOVE SAVE-ADLAGTOM-WDT1I-E  TO W-ADLAGTOM-WDT1I-MIN           
157900            MOVE SAVE-KDPRIO-WDT1I-E    TO W-KDPRIO-WDT1I-MIN             
158000            MOVE SAVE-ADGANGFOM-WDT1I-E TO W-ADGANGFOM-WDT1I-MIN          
158100            MOVE SAVE-ADPLATFOM-WDT1I-E TO W-ADPLATFOM-WDT1I-MIN          
158200            MOVE SAVE-KDSTAPF-WDT1I-E   TO W-KDSTAPF-WDT1I-MIN            
158300            MOVE SAVE-TIORDTIME-WDT1I-E TO W-TIORDTIME-WDT1I-MIN          
158400          WHEN WDT101-CHECK                                               
158500            MOVE SAVE-IDDC-WDT101-E      TO W-IDDC-WDT101-MIN             
158600            MOVE SAVE-IDARTNR-WDT101-E   TO W-IDARTNR-WDT101-MIN          
158700            MOVE SAVE-TIORDTIME-WDT101-E TO W-TIORDTIME-WDT101-MIN        
158800          END-EVALUATE                                                    
158900          PERFORM MFS-ERASE-FIELD-IN                                      
159000       END-IF                                                             
159100     ELSE                                                                 
159200       PERFORM MFS-ERASE-FIELD-IN                                         
159300     END-IF                                                               
159400     .                                                                    
159500     EJECT                                                                
159600                                                                          
159700 F-READ-SHOW-INFO SECTION.                                                
159800     IF WDT101-CHECK                                                      
159900       PERFORM F-READ-BASICDATA-IDARTNO                                   
160000       IF SEGMENT-MISSING                                                 
160100         MOVE W-WDT101KY-MIN-X    TO SAVE-WDT101KY-ENTER                  
160200                                     SAVE-WDT101KY-NEXT                   
160300         MOVE INF-NO-RECORDS        TO MED-IDMFSINF                       
160400         CALL WMEDKONV USING MED-WMEDAREA                                 
160500         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
160600       ELSE                                                               
160700         MOVE PF-IDDC             TO SAVE-IDDC-WDT101-E                   
160800         MOVE PF-IDARTNR          TO SAVE-IDARTNR-WDT101-E                
160900         MOVE PF-TIORDTIME        TO SAVE-TIORDTIME-WDT101-E              
161000         MOVE SAVE-WDT101KY-ENTER TO W-WDT101KY-X                         
161100         MOVE +1 TO INDX                                                  
161200         PERFORM UNTIL INDX > MAX-INDX                                    
161300           IF SEGMENT-FOUND                                               
161400             IF PF-KDSTAPF NOT = 'A'                                      
161500               PERFORM XX-MOVE-WDT101-TO-MOD                              
161600               ADD +1 TO INDX                                             
161700             END-IF                                                       
161800             IF (W-IDARTNR > 0      AND                                   
161900                W-ADLAGOMR-FOM IS NUMERIC  AND                            
162000                (W-KDSTAPF = 'R' OR 'V' OR 'L' OR 'E'))                   
162100               PERFORM IMS-GN-WDT101-ADLAGFOM-STA                         
162200             ELSE                                                         
162300               IF (W-IDARTNR > 0         AND                              
162400                  W-ADLAGOMR-TOM IS NUMERIC   AND                         
162500                  (W-KDSTAPF = 'R' OR 'V' OR 'L' OR 'E'))                 
162600                 PERFORM IMS-GN-WDT101-ADLAGTOM-STA                       
162700               ELSE                                                       
162800                 IF W-IDARTNR > 0        AND                              
162900                   W-ADLAGOMR-FOM IS NUMERIC                              
163000                   PERFORM IMS-GN-WDT101-ADLAGFOM                         
163100                 ELSE                                                     
163200                   IF W-IDARTNR > 0      AND                              
163300                     W-ADLAGOMR-TOM IS NUMERIC                            
163400                     PERFORM IMS-GN-WDT101-ADLAGTOM                       
163500                   ELSE                                                   
163600                     IF (W-IDARTNR > 0   AND                              
163700                        (W-KDSTAPF = 'R' OR 'V' OR 'L' OR 'E'))           
163800                       PERFORM IMS-GN-WDT101-STATUS                       
163900                     ELSE                                                 
164000                       IF W-IDARTNR > 0                                   
164100                         PERFORM IMS-GN-WDT101                            
164200                       END-IF                                             
164300                     END-IF                                               
164400                   END-IF                                                 
164500                 END-IF                                                   
164600               END-IF                                                     
164700             END-IF                                                       
164800           ELSE                                                           
164900             PERFORM XX-CLOSE-ERASE-FIELD                                 
165000             ADD +1 TO INDX                                               
165100           END-IF                                                         
165200         END-PERFORM                                                      
165300                                                                          
165400         IF SEGMENT-FOUND                                                 
165500           MOVE PF-IDDC            TO SAVE-IDDC-WDT101-N                  
165600           MOVE PF-IDARTNR         TO SAVE-IDARTNR-WDT101-N               
165700           MOVE PF-TIORDTIME       TO SAVE-TIORDTIME-WDT101-N             
165800           MOVE SAVE-WDT101KY-NEXT TO W-WDT101KY-MIN-X                    
165900           IF MFS-UPDATE OR MFS-PRINT                                     
166000             CONTINUE                                                     
166100           ELSE                                                           
166200             MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                    
166300             CALL WMEDKONV USING MED-WMEDAREA                             
166400             MOVE MED-TEMFSINF TO MOD-TEMFSINF                            
166500           END-IF                                                         
166600         ELSE                                                             
166700           MOVE W-WDT101KY-MIN-X TO SAVE-WDT101KY-NEXT                    
166800         END-IF                                                           
166900       END-IF                                                             
167000     END-IF                                                               
167100                                                                          
167200     IF WDT1A1-CHECK                                                      
167300       PERFORM FA-READ-BASICDATA                                          
167400       IF SEGMENT-MISSING                                                 
167500         MOVE W-WDT1A1KY-MIN-X  TO SAVE-AREA-ENTER                        
167600                                   SAVE-AREA-NEXT                         
167700         MOVE INF-NO-RECORDS        TO MED-IDMFSINF                       
167800         CALL WMEDKONV USING MED-WMEDAREA                                 
167900         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
168000       ELSE                                                               
168100         MOVE SEQA-IDDC         TO SAVE-IDDC-ENTER                        
168200         MOVE SEQA-ADLAGOMR-FOM TO SAVE-ADLAGOMR-ADR-ENTER                
168300         MOVE SEQA-KDSTAPF      TO SAVE-KDSTAPF-ENTER                     
168400         MOVE SEQA-KDPRIO-PF    TO SAVE-KDPRIO-PF-ENTER                   
168500         MOVE SEQA-TIORDTIME    TO SAVE-TIORDTIME-ENTER                   
168600         MOVE SAVE-AREA-ENTER   TO W-WDT1A1KY-MIN-X                       
168700         MOVE SEQA-IDWDT101     TO W-WDT101KY-X                           
168800         MOVE +1 TO INDX                                                  
168900         PERFORM FB-READ-LINEDATA                                         
169000         PERFORM UNTIL INDX > MAX-INDX                                    
169100         IF SEGMENT-FOUND                                                 
169200           PERFORM XX-MOVE-WDT101-TO-MOD                                  
169300           IF W-ADLAGOMR-FOM IS NOT NUMERIC                               
169400           AND (W-KDSTAPF = 'R' OR 'V' OR 'L' OR 'E')                     
169500              PERFORM IMS-GN-WDT1A1-STATUS                                
169600           ELSE                                                           
169700              PERFORM IMS-GN-WDT1A1                                       
169800           END-IF                                                         
169900           IF SEGMENT-FOUND                                               
170000              MOVE SEQA-IDWDT101 TO W-WDT101KY-X                          
170100              PERFORM IMS-GHU-WDT101                                      
170200           END-IF                                                         
170300         ELSE                                                             
170400           PERFORM XX-CLOSE-ERASE-FIELD                                   
170500         END-IF                                                           
170600         ADD +1 TO INDX                                                   
170700         END-PERFORM                                                      
170800                                                                          
170900         IF SEGMENT-FOUND                                                 
171000           MOVE SEQA-IDDC         TO SAVE-IDDC-NEXT                       
171100           MOVE SEQA-ADLAGOMR-FOM TO SAVE-ADLAGOMR-ADR-NEXT               
171200           MOVE SEQA-KDSTAPF      TO SAVE-KDSTAPF-NEXT                    
171300           MOVE SEQA-KDPRIO-PF    TO SAVE-KDPRIO-PF-NEXT                  
171400           MOVE SEQA-TIORDTIME    TO SAVE-TIORDTIME-NEXT                  
171500           MOVE SAVE-AREA-NEXT    TO W-WDT1A1KY-MIN-X                     
171600           IF MFS-UPDATE OR MFS-PRINT                                     
171700             CONTINUE                                                     
171800           ELSE                                                           
171900             MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                    
172000             CALL WMEDKONV USING MED-WMEDAREA                             
172100             MOVE MED-TEMFSINF TO MOD-TEMFSINF                            
172200           END-IF                                                         
172300         ELSE                                                             
172400           MOVE W-WDT1A1KY-MIN-X TO SAVE-AREA-NEXT                        
172500         END-IF                                                           
172600       END-IF                                                             
172700     END-IF                                                               
172800                                                                          
172900     IF WDT1B1-CHECK                                                      
173000       PERFORM FB-READ-BASICDATA                                          
173100       IF SEGMENT-MISSING                                                 
173200         MOVE W-WDT1B1KY-MIN-X  TO SAVE-AREA-ENTER                        
173300                                   SAVE-AREA-NEXT                         
173400         MOVE INF-NO-RECORDS    TO MED-IDMFSINF                           
173500         CALL WMEDKONV USING MED-WMEDAREA                                 
173600         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
173700       ELSE                                                               
173800         MOVE SEQB-IDDC         TO SAVE-IDDC-ENTER                        
173900         MOVE SEQB-ADLAGOMR-TOM TO SAVE-ADLAGOMR-ADR-ENTER                
174000         MOVE SEQB-KDSTAPF      TO SAVE-KDSTAPF-ENTER                     
174100         MOVE SEQB-KDPRIO-PF    TO SAVE-KDPRIO-PF-ENTER                   
174200         MOVE SEQB-TIORDTIME    TO SAVE-TIORDTIME-ENTER                   
174300         MOVE SAVE-AREA-ENTER   TO W-WDT1B1KY-MIN-X                       
174400         MOVE SEQB-IDWDT101     TO W-WDT101KY-X                           
174500         MOVE +1 TO INDX                                                  
174600         PERFORM FB-READ-LINEDATA                                         
174700         PERFORM UNTIL INDX > MAX-INDX                                    
174800         IF SEGMENT-FOUND                                                 
174900           PERFORM XX-MOVE-WDT101-TO-MOD                                  
175000           IF W-ADLAGOMR-TOM IS NOT NUMERIC                               
175100           AND (W-KDSTAPF = 'R' OR 'V' OR 'L' OR 'E')                     
175200               PERFORM IMS-GN-WDT1B1-STATUS                               
175300           ELSE                                                           
175400               PERFORM IMS-GN-WDT1B1                                      
175500           END-IF                                                         
175600           IF SEGMENT-FOUND                                               
175700              MOVE SEQB-IDWDT101 TO W-WDT101KY-X                          
175800              PERFORM IMS-GHU-WDT101                                      
175900           END-IF                                                         
176000         ELSE                                                             
176100           PERFORM XX-CLOSE-ERASE-FIELD                                   
176200         END-IF                                                           
176300         ADD 1 TO INDX                                                    
176400       END-PERFORM                                                        
176500                                                                          
176600       IF SEGMENT-FOUND                                                   
176700         MOVE SEQB-IDDC         TO SAVE-IDDC-NEXT                         
176800         MOVE SEQB-ADLAGOMR-TOM TO SAVE-ADLAGOMR-ADR-NEXT                 
176900         MOVE SEQB-KDSTAPF      TO SAVE-KDSTAPF-NEXT                      
177000         MOVE SEQB-KDPRIO-PF    TO SAVE-KDPRIO-PF-NEXT                    
177100         MOVE SEQB-TIORDTIME    TO SAVE-TIORDTIME-NEXT                    
177200         MOVE SAVE-AREA-NEXT    TO W-WDT1B1KY-MIN-X                       
177300         IF MFS-UPDATE OR MFS-PRINT                                       
177400           CONTINUE                                                       
177500         ELSE                                                             
177600           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
177700           CALL WMEDKONV USING MED-WMEDAREA                               
177800           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
177900         END-IF                                                           
178000       ELSE                                                               
178100         MOVE W-WDT1B1KY-MIN-X TO SAVE-AREA-NEXT                          
178200       END-IF                                                             
178300      END-IF                                                              
178400     END-IF                                                               
178500                                                                          
178600     IF WDT1C1-CHECK                                                      
178700       PERFORM FC-READ-BASICDATA                                          
178800       IF SEGMENT-MISSING                                                 
178900         MOVE W-WDT1C1KY-MIN-X  TO SAVE-WDT1C1KY-ENTER                    
179000                                   SAVE-WDT1C1KY-NEXT                     
179100         MOVE INF-NO-RECORDS    TO MED-IDMFSINF                           
179200         CALL WMEDKONV USING MED-WMEDAREA                                 
179300         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
179400       ELSE                                                               
179500         MOVE SEQC-IDDC         TO SAVE-IDDC-WDT1C-E                      
179600         MOVE SEQC-ADLAGOMR-FOM TO SAVE-ADLAGFOM-WDT1C-E                  
179700         MOVE SEQC-ADGANG-FOM   TO SAVE-ADGANGFOM-WDT1C-E                 
179800         MOVE SEQC-ADPLATS-FOM  TO SAVE-ADPLATFOM-WDT1C-E                 
179900         MOVE SEQC-TIORDTIME    TO SAVE-TIORDTIME-WDT1C-E                 
180000         MOVE SAVE-WDT1C1KY-ENTER TO W-WDT1C1KY-MIN-X                     
180100         MOVE SEQC-IDWDT101     TO W-WDT101KY-X                           
180200         MOVE +1 TO INDX                                                  
180300         PERFORM FB-READ-LINEDATA                                         
180400         PERFORM UNTIL INDX > MAX-INDX                                    
180500          IF SEGMENT-FOUND                                                
180600            PERFORM XX-MOVE-WDT101-TO-MOD                                 
180700***         PERFORM IMS-GN-WDT1C1                                         
180800            IF W-ADLAGOMR-FOM IS NUMERIC                                  
180900            AND W-ADGANG-FOM IS NUMERIC                                   
181000            AND (W-KDSTAPF = 'R' OR 'V' OR 'L')                           
181100               PERFORM IMS-GN-WDT1C1-STATUS                               
181200            ELSE                                                          
181300               PERFORM IMS-GN-WDT1C1                                      
181400            END-IF                                                        
181500            IF SEGMENT-FOUND                                              
181600              MOVE SEQC-IDWDT101 TO W-WDT101KY-X                          
181700              PERFORM IMS-GHU-WDT101                                      
181800            END-IF                                                        
181900          ELSE                                                            
182000            PERFORM XX-CLOSE-ERASE-FIELD                                  
182100          END-IF                                                          
182200          ADD 1 TO INDX                                                   
182300         END-PERFORM                                                      
182400                                                                          
182500        IF SEGMENT-FOUND                                                  
182600          MOVE SEQC-IDDC          TO SAVE-IDDC-WDT1C-N                    
182700          MOVE SEQC-ADLAGOMR-FOM  TO SAVE-ADLAGFOM-WDT1C-N                
182800          MOVE SEQC-ADGANG-FOM    TO SAVE-ADGANGFOM-WDT1C-N               
182900          MOVE SEQC-ADPLATS-FOM   TO SAVE-ADPLATFOM-WDT1C-N               
183000          MOVE SEQC-TIORDTIME     TO SAVE-TIORDTIME-WDT1C-N               
183100          MOVE SAVE-WDT1C1KY-NEXT TO W-WDT1C1KY-MIN-X                     
183200          IF MFS-UPDATE OR MFS-PRINT                                      
183300            CONTINUE                                                      
183400          ELSE                                                            
183500            MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                     
183600            CALL WMEDKONV USING MED-WMEDAREA                              
183700            MOVE MED-TEMFSINF TO MOD-TEMFSINF                             
183800          END-IF                                                          
183900        ELSE                                                              
184000          MOVE W-WDT1C1KY-MIN-X TO SAVE-WDT1C1KY-NEXT                     
184100        END-IF                                                            
184200      END-IF                                                              
184300     END-IF                                                               
184400                                                                          
184500     IF WDT1D1-CHECK                                                      
184600       PERFORM FD-READ-BASICDATA                                          
184700       IF SEGMENT-MISSING                                                 
184800         MOVE W-WDT1D1KY-MIN-X  TO SAVE-WDT1D1KY-ENTER                    
184900                                   SAVE-WDT1D1KY-NEXT                     
185000         MOVE INF-NO-RECORDS    TO MED-IDMFSINF                           
185100         CALL WMEDKONV USING MED-WMEDAREA                                 
185200         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
185300       ELSE                                                               
185400         MOVE SEQD-IDDC         TO SAVE-IDDC-WDT1D-E                      
185500         MOVE SEQD-ADLAGOMR-TOM TO SAVE-ADLAGTOM-WDT1D-E                  
185600         MOVE SEQD-ADGANG-TOM   TO SAVE-ADGANGTOM-WDT1D-E                 
185700         MOVE SEQD-ADPLATS-TOM  TO SAVE-ADPLATTOM-WDT1D-E                 
185800         MOVE SEQD-TIORDTIME    TO SAVE-TIORDTIME-WDT1D-E                 
185900         MOVE SAVE-WDT1D1KY-ENTER TO W-WDT1D1KY-MIN-X                     
186000         MOVE SEQD-IDWDT101     TO W-WDT101KY-X                           
186100         MOVE +1 TO INDX                                                  
186200         PERFORM FB-READ-LINEDATA                                         
186300         PERFORM UNTIL INDX > MAX-INDX                                    
186400          IF SEGMENT-FOUND                                                
186500            PERFORM XX-MOVE-WDT101-TO-MOD                                 
186600**          PERFORM IMS-GN-WDT1D1                                         
186700            IF W-ADLAGOMR-TOM IS NUMERIC AND                              
186800               W-ADGANG-TOM   IS NUMERIC AND                              
186900               (W-KDSTAPF = 'R' OR 'V' OR 'L' OR 'E')                     
187000              PERFORM IMS-GN-WDT1D1-STATUS                                
187100            ELSE                                                          
187200              PERFORM IMS-GN-WDT1D1                                       
187300            END-IF                                                        
187400            IF SEGMENT-FOUND                                              
187500              MOVE SEQD-IDWDT101 TO W-WDT101KY-X                          
187600              PERFORM IMS-GHU-WDT101                                      
187700            END-IF                                                        
187800          ELSE                                                            
187900            PERFORM XX-CLOSE-ERASE-FIELD                                  
188000          END-IF                                                          
188100          ADD 1 TO INDX                                                   
188200         END-PERFORM                                                      
188300                                                                          
188400         IF SEGMENT-FOUND                                                 
188500           MOVE SEQD-IDDC          TO SAVE-IDDC-WDT1D-N                   
188600           MOVE SEQD-ADLAGOMR-TOM  TO SAVE-ADLAGTOM-WDT1D-N               
188700           MOVE SEQD-ADGANG-TOM    TO SAVE-ADGANGTOM-WDT1D-N              
188800           MOVE SEQD-ADPLATS-TOM   TO SAVE-ADPLATTOM-WDT1D-N              
188900           MOVE SEQD-TIORDTIME     TO SAVE-TIORDTIME-WDT1D-N              
189000           MOVE SAVE-WDT1D1KY-NEXT TO W-WDT1D1KY-MIN-X                    
189100           IF MFS-UPDATE OR MFS-PRINT                                     
189200             CONTINUE                                                     
189300           ELSE                                                           
189400             MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                    
189500             CALL WMEDKONV USING MED-WMEDAREA                             
189600             MOVE MED-TEMFSINF         TO MOD-TEMFSINF                    
189700           END-IF                                                         
189800         ELSE                                                             
189900           MOVE W-WDT1D1KY-MIN-X TO SAVE-WDT1D1KY-NEXT                    
190000         END-IF                                                           
190100       END-IF                                                             
190200     END-IF                                                               
190300                                                                          
190400     IF WDT1E1-CHECK                                                      
190500       PERFORM FE-READ-BASICDATA                                          
190600       IF SEGMENT-MISSING                                                 
190700         MOVE W-WDT1E1KY-MIN-X  TO SAVE-WDT1E1KY-ENTER                    
190800                                   SAVE-WDT1E1KY-NEXT                     
190900         MOVE INF-NO-RECORDS    TO MED-IDMFSINF                           
191000         CALL WMEDKONV USING MED-WMEDAREA                                 
191100         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
191200       ELSE                                                               
191300         MOVE SEQE-IDDC         TO SAVE-IDDC-WDT1E-E                      
191400         MOVE SEQE-IDUSER       TO SAVE-IDUSER-WDT1E-E                    
191500         MOVE SEQE-ADLAGOMR-FOM TO SAVE-ADLAGFOM-WDT1E-E                  
191600         MOVE SEQE-ADGANG-FOM   TO SAVE-ADGANGFOM-WDT1E-E                 
191700         MOVE SEQE-ADPLATS-FOM  TO SAVE-ADPLATFOM-WDT1E-E                 
191800         MOVE SEQE-KDSTAPF      TO SAVE-KDSTAPF-WDT1E-E                   
191900         MOVE SEQE-TIORDTIME    TO SAVE-TIORDTIME-WDT1E-E                 
192000         MOVE SAVE-WDT1E1KY-ENTER TO W-WDT1E1KY-MIN-X                     
192100         MOVE SEQE-IDWDT101     TO W-WDT101KY-X                           
192200         MOVE +1 TO INDX                                                  
192300         PERFORM FB-READ-LINEDATA                                         
192400         PERFORM UNTIL INDX > MAX-INDX                                    
192500          IF SEGMENT-FOUND                                                
192600            PERFORM XX-MOVE-WDT101-TO-MOD                                 
192700            IF ((W-IDUSER > SPACES     AND                                
192800               W-ADLAGOMR-FOM IS NUMERIC AND                              
192900               (W-KDSTAPF = 'R' OR 'V' OR 'L' OR 'E'))                    
193000               OR                                                         
193100               (W-IDUSER > SPACES AND                                     
193200               (W-KDSTAPF = 'R' OR 'V' OR 'L' OR 'E')))                   
193300              PERFORM IMS-GN-WDT1E1-STATUS                                
193400            ELSE                                                          
193500              IF ((W-IDUSER > SPACES  AND                                 
193600                 W-ADLAGOMR-FOM IS NUMERIC)                               
193700                 OR                                                       
193800                 W-IDUSER > SPACES)                                       
193900                 PERFORM IMS-GN-WDT1E1                                    
194000              END-IF                                                      
194100            END-IF                                                        
194200            IF SEGMENT-FOUND                                              
194300               MOVE SEQE-IDWDT101 TO W-WDT101KY-X                         
194400               PERFORM IMS-GHU-WDT101                                     
194500            END-IF                                                        
194600          ELSE                                                            
194700            PERFORM XX-CLOSE-ERASE-FIELD                                  
194800          END-IF                                                          
194900          ADD 1 TO INDX                                                   
195000         END-PERFORM                                                      
195100                                                                          
195200         IF SEGMENT-FOUND                                                 
195300           MOVE SEQE-IDDC         TO SAVE-IDDC-WDT1E-N                    
195400           MOVE SEQE-IDUSER       TO SAVE-IDUSER-WDT1E-N                  
195500           MOVE SEQE-ADLAGOMR-FOM TO SAVE-ADLAGFOM-WDT1E-N                
195600           MOVE SEQE-ADGANG-FOM   TO SAVE-ADGANGFOM-WDT1E-N               
195700           MOVE SEQE-ADPLATS-FOM  TO SAVE-ADPLATFOM-WDT1E-N               
195800           MOVE SEQE-KDSTAPF      TO SAVE-KDSTAPF-WDT1E-N                 
195900           MOVE SEQE-TIORDTIME    TO SAVE-TIORDTIME-WDT1E-N               
196000           MOVE SAVE-WDT1E1KY-NEXT TO W-WDT1E1KY-MIN-X                    
196100           IF MFS-UPDATE OR MFS-PRINT                                     
196200              CONTINUE                                                    
196300           ELSE                                                           
196400              MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                   
196500              CALL WMEDKONV USING MED-WMEDAREA                            
196600              MOVE MED-TEMFSINF         TO MOD-TEMFSINF                   
196700           END-IF                                                         
196800         ELSE                                                             
196900           MOVE W-WDT1E1KY-MIN-X TO SAVE-WDT1E1KY-NEXT                    
197000         END-IF                                                           
197100       END-IF                                                             
197200     END-IF                                                               
197300                                                                          
197400     IF WDT1F1-CHECK                                                      
197500       PERFORM FF-READ-BASICDATA                                          
197600       IF SEGMENT-MISSING                                                 
197700         MOVE W-WDT1F1KY-MIN-X  TO SAVE-WDT1F1KY-ENTER                    
197800                                   SAVE-WDT1F1KY-NEXT                     
197900         MOVE INF-NO-RECORDS    TO MED-IDMFSINF                           
198000         CALL WMEDKONV USING MED-WMEDAREA                                 
198100         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
198200       ELSE                                                               
198300         MOVE SEQF-IDDC         TO SAVE-IDDC-WDT1F-E                      
198400         MOVE SEQF-IDUSER       TO SAVE-IDUSER-WDT1F-E                    
198500         MOVE SEQF-ADLAGOMR-TOM TO SAVE-ADLAGTOM-WDT1F-E                  
198600         MOVE SEQF-ADGANG-TOM   TO SAVE-ADGANGTOM-WDT1F-E                 
198700         MOVE SEQF-ADPLATS-TOM  TO SAVE-ADPLATTOM-WDT1F-E                 
198800         MOVE SEQF-KDSTAPF      TO SAVE-KDSTAPF-WDT1F-E                   
198900         MOVE SEQF-TIORDTIME    TO SAVE-TIORDTIME-WDT1F-E                 
199000         MOVE SAVE-WDT1F1KY-ENTER TO W-WDT1F1KY-MIN-X                     
199100         MOVE SEQF-IDWDT101     TO W-WDT101KY-X                           
199200         MOVE +1 TO INDX                                                  
199300         PERFORM FB-READ-LINEDATA                                         
199400         PERFORM UNTIL INDX > MAX-INDX                                    
199500          IF SEGMENT-FOUND                                                
199600            PERFORM XX-MOVE-WDT101-TO-MOD                                 
199700            IF (W-IDUSER > SPACES     AND                                 
199800                W-ADLAGOMR-TOM IS NUMERIC AND                             
199900               (W-KDSTAPF = 'R' OR 'V' OR 'L' OR 'E'))                    
200000              PERFORM IMS-GN-WDT1F1-STATUS                                
200100            ELSE                                                          
200200              IF (W-IDUSER > SPACES  AND                                  
200300                 W-ADLAGOMR-TOM IS NUMERIC)                               
200400                 PERFORM IMS-GN-WDT1F1                                    
200500              END-IF                                                      
200600            END-IF                                                        
200700            IF SEGMENT-FOUND                                              
200800               MOVE SEQF-IDWDT101 TO W-WDT101KY-X                         
200900               PERFORM IMS-GHU-WDT101                                     
201000            END-IF                                                        
201100          ELSE                                                            
201200            PERFORM XX-CLOSE-ERASE-FIELD                                  
201300          END-IF                                                          
201400          ADD 1 TO INDX                                                   
201500         END-PERFORM                                                      
201600                                                                          
201700         IF SEGMENT-FOUND                                                 
201800           MOVE SEQF-IDDC         TO SAVE-IDDC-WDT1F-N                    
201900           MOVE SEQF-IDUSER       TO SAVE-IDUSER-WDT1F-N                  
202000           MOVE SEQF-ADLAGOMR-TOM TO SAVE-ADLAGTOM-WDT1F-N                
202100           MOVE SEQF-ADGANG-TOM   TO SAVE-ADGANGTOM-WDT1F-N               
202200           MOVE SEQF-ADPLATS-TOM  TO SAVE-ADPLATTOM-WDT1F-N               
202300           MOVE SEQF-KDSTAPF      TO SAVE-KDSTAPF-WDT1F-N                 
202400           MOVE SEQF-TIORDTIME    TO SAVE-TIORDTIME-WDT1F-N               
202500           MOVE SAVE-WDT1F1KY-NEXT TO W-WDT1F1KY-MIN-X                    
202600           IF MFS-UPDATE OR MFS-PRINT                                     
202700              CONTINUE                                                    
202800           ELSE                                                           
202900              MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                   
203000              CALL WMEDKONV USING MED-WMEDAREA                            
203100              MOVE MED-TEMFSINF         TO MOD-TEMFSINF                   
203200           END-IF                                                         
203300         ELSE                                                             
203400           MOVE W-WDT1F1KY-MIN-X TO SAVE-WDT1F1KY-NEXT                    
203500         END-IF                                                           
203600       END-IF                                                             
203700     END-IF                                                               
203800                                                                          
203900     IF WDT1G1-CHECK                                                      
204000        PERFORM FG-READ-BASICDATA                                         
204100        IF SEGMENT-MISSING                                                
204200           MOVE W-WDT1G1KY-MIN-X  TO SAVE-WDT1G1KY-ENTER                  
204300                                     SAVE-WDT1G1KY-NEXT                   
204400           MOVE INF-NO-RECORDS    TO MED-IDMFSINF                         
204500           CALL WMEDKONV USING MED-WMEDAREA                               
204600           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
204700        ELSE                                                              
204800           MOVE SEQG-IDDC         TO SAVE-IDDC-WDT1G-E                    
204900           MOVE SEQG-KDPRIO-PF    TO SAVE-KDPRIO-WDT1G-E                  
205000           MOVE SEQG-ADLAGOMR-FOM TO SAVE-ADLAGFOM-WDT1G-E                
205100           MOVE SEQG-ADGANG-FOM   TO SAVE-ADGANGFOM-WDT1G-E               
205200           MOVE SEQG-ADPLATS-FOM  TO SAVE-ADPLATFOM-WDT1G-E               
205300           MOVE SEQG-KDSTAPF      TO SAVE-KDSTAPF-WDT1G-E                 
205400           MOVE SEQG-TIORDTIME    TO SAVE-TIORDTIME-WDT1G-E               
205500           MOVE SAVE-WDT1G1KY-ENTER TO W-WDT1G1KY-MIN-X                   
205600           MOVE SEQG-IDWDT101     TO W-WDT101KY-X                         
205700           MOVE +1 TO INDX                                                
205800           PERFORM FB-READ-LINEDATA                                       
205900           PERFORM UNTIL INDX > MAX-INDX                                  
206000            IF SEGMENT-FOUND                                              
206100              PERFORM XX-MOVE-WDT101-TO-MOD                               
206200              IF ((W-KDPRIO-PF > SPACES     AND                           
206300                 W-ADLAGOMR-FOM IS NUMERIC AND                            
206400                 (W-KDSTAPF = 'R' OR 'V' OR 'L' OR 'E'))                  
206500                 OR                                                       
206600                 (W-KDPRIO-PF > SPACES AND                                
206700                 (W-KDSTAPF = 'R' OR 'V' OR 'L' OR 'E')))                 
206800                PERFORM IMS-GN-WDT1G1-STATUS                              
206900              ELSE                                                        
207000                IF ((W-KDPRIO-PF > SPACES  AND                            
207100                    W-ADLAGOMR-FOM IS NUMERIC)                            
207200                    OR                                                    
207300                    W-KDPRIO-PF > SPACES)                                 
207400                   PERFORM IMS-GN-WDT1G1                                  
207500                END-IF                                                    
207600              END-IF                                                      
207700              IF SEGMENT-FOUND                                            
207800                MOVE SEQG-IDWDT101 TO W-WDT101KY-X                        
207900                PERFORM IMS-GHU-WDT101                                    
208000              END-IF                                                      
208100            ELSE                                                          
208200              PERFORM XX-CLOSE-ERASE-FIELD                                
208300            END-IF                                                        
208400            ADD 1 TO INDX                                                 
208500           END-PERFORM                                                    
208600                                                                          
208700          IF SEGMENT-FOUND                                                
208800            MOVE SEQG-IDDC         TO SAVE-IDDC-WDT1G-N                   
208900            MOVE SEQG-KDPRIO-PF    TO SAVE-KDPRIO-WDT1G-N                 
209000            MOVE SEQG-ADLAGOMR-FOM TO SAVE-ADLAGFOM-WDT1G-N               
209100            MOVE SEQG-ADGANG-FOM   TO SAVE-ADGANGFOM-WDT1G-N              
209200            MOVE SEQG-ADPLATS-FOM  TO SAVE-ADPLATFOM-WDT1G-N              
209300            MOVE SEQG-KDSTAPF      TO SAVE-KDSTAPF-WDT1G-N                
209400            MOVE SEQG-TIORDTIME    TO SAVE-TIORDTIME-WDT1G-N              
209500            MOVE SAVE-WDT1G1KY-NEXT TO W-WDT1G1KY-MIN-X                   
209600            IF MFS-UPDATE OR MFS-PRINT                                    
209700              CONTINUE                                                    
209800            ELSE                                                          
209900              MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                   
210000              CALL WMEDKONV USING MED-WMEDAREA                            
210100              MOVE MED-TEMFSINF         TO MOD-TEMFSINF                   
210200            END-IF                                                        
210300          ELSE                                                            
210400            MOVE W-WDT1G1KY-MIN-X TO SAVE-WDT1G1KY-NEXT                   
210500          END-IF                                                          
210600         END-IF                                                           
210700       END-IF                                                             
210800                                                                          
210900     IF WDT1H1-CHECK                                                      
211000       PERFORM FH-READ-BASICDATA                                          
211100       IF SEGMENT-MISSING                                                 
211200         MOVE W-WDT1H1KY-MIN-X  TO SAVE-WDT1H1KY-ENTER                    
211300                                   SAVE-WDT1H1KY-NEXT                     
211400         MOVE INF-NO-RECORDS    TO MED-IDMFSINF                           
211500         CALL WMEDKONV USING MED-WMEDAREA                                 
211600         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
211700       ELSE                                                               
211800         MOVE SEQH-IDDC         TO SAVE-IDDC-WDT1H-E                      
211900         MOVE SEQH-KDPRIO-PF    TO SAVE-KDPRIO-WDT1H-E                    
212000         MOVE SEQH-ADLAGOMR-TOM TO SAVE-ADLAGTOM-WDT1H-E                  
212100         MOVE SEQH-ADGANG-TOM   TO SAVE-ADGANGTOM-WDT1H-E                 
212200         MOVE SEQH-ADPLATS-TOM  TO SAVE-ADPLATTOM-WDT1H-E                 
212300         MOVE SEQH-KDSTAPF      TO SAVE-KDSTAPF-WDT1H-E                   
212400         MOVE SEQH-TIORDTIME    TO SAVE-TIORDTIME-WDT1H-E                 
212500         MOVE SAVE-WDT1H1KY-ENTER TO W-WDT1H1KY-MIN-X                     
212600         MOVE SEQH-IDWDT101     TO W-WDT101KY-X                           
212700         MOVE +1 TO INDX                                                  
212800         PERFORM FB-READ-LINEDATA                                         
212900         PERFORM UNTIL INDX > MAX-INDX                                    
213000          IF SEGMENT-FOUND                                                
213100            PERFORM XX-MOVE-WDT101-TO-MOD                                 
213200            IF (W-KDPRIO-PF > SPACES     AND                              
213300               W-ADLAGOMR-TOM IS NUMERIC AND                              
213400               (W-KDSTAPF = 'R' OR 'V' OR 'L' OR 'E'))                    
213500               PERFORM IMS-GN-WDT1H1-STATUS                               
213600            ELSE                                                          
213700               IF (W-KDPRIO-PF > SPACES  AND                              
213800                  W-ADLAGOMR-TOM IS NUMERIC)                              
213900                  PERFORM IMS-GN-WDT1H1                                   
214000               END-IF                                                     
214100            END-IF                                                        
214200            IF SEGMENT-FOUND                                              
214300               MOVE SEQH-IDWDT101 TO W-WDT101KY-X                         
214400                PERFORM IMS-GHU-WDT101                                    
214500            END-IF                                                        
214600          ELSE                                                            
214700            PERFORM XX-CLOSE-ERASE-FIELD                                  
214800          END-IF                                                          
214900          ADD 1 TO INDX                                                   
215000         END-PERFORM                                                      
215100                                                                          
215200         IF SEGMENT-FOUND                                                 
215300           MOVE SEQH-IDDC         TO SAVE-IDDC-WDT1H-N                    
215400           MOVE SEQH-KDPRIO-PF    TO SAVE-KDPRIO-WDT1H-N                  
215500           MOVE SEQH-ADLAGOMR-TOM TO SAVE-ADLAGTOM-WDT1H-N                
215600           MOVE SEQH-ADGANG-TOM   TO SAVE-ADGANGTOM-WDT1H-N               
215700           MOVE SEQH-ADPLATS-TOM  TO SAVE-ADPLATTOM-WDT1H-N               
215800           MOVE SEQH-KDSTAPF      TO SAVE-KDSTAPF-WDT1H-N                 
215900           MOVE SEQH-TIORDTIME    TO SAVE-TIORDTIME-WDT1H-N               
216000           MOVE SAVE-WDT1H1KY-NEXT TO W-WDT1H1KY-MIN-X                    
216100           IF MFS-UPDATE OR MFS-PRINT                                     
216200              CONTINUE                                                    
216300            ELSE                                                          
216400              MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                   
216500              CALL WMEDKONV USING MED-WMEDAREA                            
216600              MOVE MED-TEMFSINF         TO MOD-TEMFSINF                   
216700           END-IF                                                         
216800         ELSE                                                             
216900           MOVE W-WDT1H1KY-MIN-X TO SAVE-WDT1H1KY-NEXT                    
217000         END-IF                                                           
217100       END-IF                                                             
217200     END-IF                                                               
217300                                                                          
217400     IF WDT1I1-CHECK                                                      
217500       PERFORM FI-READ-BASICDATA                                          
217600       IF SEGMENT-MISSING                                                 
217700         MOVE W-WDT1I1KY-MIN-X  TO SAVE-WDT1I1KY-ENTER                    
217800                                   SAVE-WDT1I1KY-NEXT                     
217900         MOVE INF-NO-RECORDS    TO MED-IDMFSINF                           
218000         CALL WMEDKONV USING MED-WMEDAREA                                 
218100         MOVE MED-TEMFSINF TO MOD-TEMFSINF                                
218200       ELSE                                                               
218300         MOVE SEQI-IDDC         TO SAVE-IDDC-WDT1I-E                      
218400         MOVE SEQI-ADLAGOMR-FOM TO SAVE-ADLAGFOM-WDT1I-E                  
218500         MOVE SEQI-ADLAGOMR-TOM TO SAVE-ADLAGTOM-WDT1I-E                  
218600         MOVE SEQI-KDPRIO-PF    TO SAVE-KDPRIO-WDT1I-E                    
218700         MOVE SEQI-ADGANG-FOM   TO SAVE-ADGANGFOM-WDT1I-E                 
218800         MOVE SEQI-ADPLATS-FOM  TO SAVE-ADPLATFOM-WDT1I-E                 
218900         MOVE SEQI-KDSTAPF      TO SAVE-KDSTAPF-WDT1I-E                   
219000         MOVE SEQI-TIORDTIME    TO SAVE-TIORDTIME-WDT1I-E                 
219100         MOVE SAVE-WDT1I1KY-ENTER TO W-WDT1I1KY-MIN-X                     
219200         MOVE SEQI-IDWDT101     TO W-WDT101KY-X                           
219300         MOVE +1 TO INDX                                                  
219400         PERFORM FB-READ-LINEDATA                                         
219500         PERFORM UNTIL INDX > MAX-INDX                                    
219600          IF SEGMENT-FOUND                                                
219700            PERFORM XX-MOVE-WDT101-TO-MOD                                 
219800            IF (W-ADLAGOMR-FOM IS NUMERIC AND                             
219900               W-ADLAGOMR-TOM IS NUMERIC AND                              
220000               (W-KDSTAPF = 'R' OR 'V' OR 'L' OR 'E'))                    
220100               PERFORM IMS-GN-WDT1I1-STATUS                               
220200            ELSE                                                          
220300               PERFORM IMS-GN-WDT1I1                                      
220400            END-IF                                                        
220500            IF SEGMENT-FOUND                                              
220600               MOVE SEQI-IDWDT101 TO W-WDT101KY-X                         
220700                PERFORM IMS-GHU-WDT101                                    
220800            END-IF                                                        
220900          ELSE                                                            
221000            PERFORM XX-CLOSE-ERASE-FIELD                                  
221100          END-IF                                                          
221200          ADD 1 TO INDX                                                   
221300         END-PERFORM                                                      
221400                                                                          
221500         IF SEGMENT-FOUND                                                 
221600           MOVE SEQI-IDDC          TO SAVE-IDDC-WDT1I-N                   
221700           MOVE SEQI-ADLAGOMR-FOM  TO SAVE-ADLAGFOM-WDT1I-N               
221800           MOVE SEQI-ADLAGOMR-TOM  TO SAVE-ADLAGTOM-WDT1I-N               
221900           MOVE SEQI-KDPRIO-PF     TO SAVE-KDPRIO-WDT1I-N                 
222000           MOVE SEQI-ADGANG-FOM    TO SAVE-ADGANGFOM-WDT1I-N              
222100           MOVE SEQI-ADPLATS-FOM   TO SAVE-ADPLATFOM-WDT1I-N              
222200           MOVE SEQI-KDSTAPF       TO SAVE-KDSTAPF-WDT1I-N                
222300           MOVE SEQI-TIORDTIME     TO SAVE-TIORDTIME-WDT1I-N              
222400           MOVE SAVE-WDT1I1KY-NEXT TO W-WDT1I1KY-MIN-X                    
222500           IF MFS-UPDATE OR MFS-PRINT                                     
222600              CONTINUE                                                    
222700            ELSE                                                          
222800              MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                   
222900              CALL WMEDKONV USING MED-WMEDAREA                            
223000              MOVE MED-TEMFSINF         TO MOD-TEMFSINF                   
223100           END-IF                                                         
223200         ELSE                                                             
223300           MOVE W-WDT1I1KY-MIN-X TO SAVE-WDT1I1KY-NEXT                    
223400         END-IF                                                           
223500       END-IF                                                             
223600     END-IF                                                               
223700                                                                          
223800     MOVE '002'      TO MSGI-KDCALL                                       
223900     MOVE '6164'     TO SAVE-IDTRANS                                      
224000     MOVE W-IDUSER   TO SAVE-IDUSER                                       
224100     MOVE SAVE-AREA  TO MSGI-SPAR-AREA                                    
224200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
224300     PERFORM MFS-ERASE-FIELD-IN                                           
224400     .                                                                    
224500     EJECT                                                                
224600                                                                          
224700 FA-READ-BASICDATA SECTION.                                               
224800     IF W-ADLAGOMR-FOM IS NOT NUMERIC                                     
224900     AND (W-KDSTAPF = 'R' OR 'V' OR 'L' OR 'E')                           
225000        PERFORM IMS-GU-WDT1A1-STATUS                                      
225100     ELSE                                                                 
225200        PERFORM IMS-GU-WDT1A1                                             
225300     END-IF                                                               
225400     .                                                                    
225500     EJECT                                                                
225600 FB-READ-BASICDATA SECTION.                                               
225700     IF W-ADLAGOMR-TOM IS NOT NUMERIC                                     
225800     AND (W-KDSTAPF = 'R' OR 'V' OR 'L' OR 'E')                           
225900        PERFORM IMS-GU-WDT1B1-STATUS                                      
226000     ELSE                                                                 
226100        PERFORM IMS-GU-WDT1B1                                             
226200     END-IF                                                               
226300     .                                                                    
226400     EJECT                                                                
226500 FC-READ-BASICDATA SECTION.                                               
226600     IF W-ADLAGOMR-FOM IS NUMERIC AND W-ADGANG-FOM IS NUMERIC             
226700     AND (W-KDSTAPF = 'R' OR 'V' OR 'L' OR 'E')                           
226800       PERFORM IMS-GU-WDT1C1-STATUS                                       
226900     ELSE                                                                 
227000       PERFORM IMS-GU-WDT1C1                                              
227100     END-IF                                                               
227200     .                                                                    
227300     EJECT                                                                
227400 FD-READ-BASICDATA SECTION.                                               
227500     IF W-ADLAGOMR-TOM IS NUMERIC AND                                     
227600        W-ADGANG-TOM   IS NUMERIC AND                                     
227700        (W-KDSTAPF = 'R' OR 'V' OR 'L' OR 'E')                            
227800       PERFORM IMS-GU-WDT1D1-STATUS                                       
227900     ELSE                                                                 
228000       PERFORM IMS-GU-WDT1D1                                              
228100     END-IF                                                               
228200     .                                                                    
228300     EJECT                                                                
228400 FE-READ-BASICDATA SECTION.                                               
228500     IF ((W-IDUSER > SPACES     AND                                       
228600        W-ADLAGOMR-FOM IS NUMERIC AND                                     
228700        (W-KDSTAPF = 'R' OR 'V' OR 'L' OR 'E'))                           
228800        OR                                                                
228900        (W-IDUSER > SPACES AND                                            
229000        (W-KDSTAPF = 'R' OR 'V' OR 'L' OR 'E')))                          
229100       PERFORM IMS-GU-WDT1E1-STATUS                                       
229200     ELSE                                                                 
229300       IF ((W-IDUSER > SPACES  AND                                        
229400          W-ADLAGOMR-FOM IS NUMERIC)                                      
229500          OR                                                              
229600          W-IDUSER > SPACES)                                              
229700          PERFORM IMS-GU-WDT1E1                                           
229800       END-IF                                                             
229900     END-IF                                                               
230000     .                                                                    
230100     EJECT                                                                
230200 FF-READ-BASICDATA SECTION.                                               
230300     IF (W-IDUSER > SPACES     AND                                        
230400        W-ADLAGOMR-TOM IS NUMERIC AND                                     
230500        (W-KDSTAPF = 'R' OR 'V' OR 'L' OR 'E'))                           
230600       PERFORM IMS-GU-WDT1F1-STATUS                                       
230700     ELSE                                                                 
230800       IF ((W-IDUSER > SPACES  AND                                        
230900          W-ADLAGOMR-TOM IS NUMERIC)                                      
231000          OR                                                              
231100          W-IDUSER > SPACES)                                              
231200         PERFORM IMS-GU-WDT1F1                                            
231300       END-IF                                                             
231400     END-IF                                                               
231500     .                                                                    
231600     EJECT                                                                
231700 FG-READ-BASICDATA SECTION.                                               
231800     IF ((W-KDPRIO-PF > SPACES     AND                                    
231900        W-ADLAGOMR-FOM IS NUMERIC AND                                     
232000        (W-KDSTAPF = 'R' OR 'V' OR 'L' OR 'E'))                           
232100        OR                                                                
232200        (W-KDPRIO-PF > SPACES AND                                         
232300        (W-KDSTAPF = 'R' OR 'V' OR 'L' OR 'E')))                          
232400       PERFORM IMS-GU-WDT1G1-STATUS                                       
232500     ELSE                                                                 
232600       IF ((W-KDPRIO-PF > SPACES  AND                                     
232700          W-ADLAGOMR-FOM IS NUMERIC)                                      
232800          OR                                                              
232900          W-KDPRIO-PF > SPACES)                                           
233000         PERFORM IMS-GU-WDT1G1                                            
233100       END-IF                                                             
233200     END-IF                                                               
233300     .                                                                    
233400     EJECT                                                                
233500 FH-READ-BASICDATA SECTION.                                               
233600     IF (W-KDPRIO-PF > SPACES     AND                                     
233700        W-ADLAGOMR-TOM IS NUMERIC AND                                     
233800        (W-KDSTAPF = 'R' OR 'V' OR 'L' OR 'E'))                           
233900       PERFORM IMS-GU-WDT1H1-STATUS                                       
234000     ELSE                                                                 
234100       IF ((W-KDPRIO-PF > SPACES  AND                                     
234200          W-ADLAGOMR-TOM IS NUMERIC)                                      
234300          OR                                                              
234400          W-KDPRIO-PF > SPACES)                                           
234500         PERFORM IMS-GU-WDT1H1                                            
234600       END-IF                                                             
234700     END-IF                                                               
234800     .                                                                    
234900     EJECT                                                                
235000 FI-READ-BASICDATA SECTION.                                               
235100     IF W-ADLAGOMR-FOM IS NUMERIC AND                                     
235200        W-ADLAGOMR-TOM IS NUMERIC                                         
235300       IF W-KDSTAPF = 'R' OR 'V' OR 'L' OR 'E'                            
235400         PERFORM IMS-GU-WDT1I1-STATUS                                     
235500       ELSE                                                               
235600         PERFORM IMS-GU-WDT1I1                                            
235700       END-IF                                                             
235800     END-IF                                                               
235900     .                                                                    
236000     EJECT                                                                
236100 F-READ-BASICDATA-IDARTNO SECTION.                                        
236200** INPUT: IDARTNO AND ADGANG-FOM/TOM KDSTAPF                              
236300**    OR  IDARTNO AND ADGANG-FOM/TOM                                      
236400**    OR  IDARTNO AND KDSTAPF                                             
236500**    OR  IDARTNO                                                         
236600     IF (W-IDARTNR > 0      AND                                           
236700        W-ADLAGOMR-FOM IS NUMERIC  AND                                    
236800        (W-KDSTAPF = 'R' OR 'V' OR 'L' OR 'E'))                           
236900       PERFORM IMS-GHU-WDT101-ADLAGFOM-STA                                
237000     ELSE                                                                 
237100       IF W-IDARTNR > 0      AND                                          
237200          W-ADLAGOMR-FOM IS NUMERIC                                       
237300         PERFORM IMS-GHU-WDT101-ADLAGFOM                                  
237400       ELSE                                                               
237500         IF (W-IDARTNR > 0      AND                                       
237600            W-ADLAGOMR-TOM IS NUMERIC  AND                                
237700            (W-KDSTAPF = 'R' OR 'V' OR 'L' OR 'E'))                       
237800           PERFORM IMS-GHU-WDT101-ADLAGTOM-STA                            
237900         ELSE                                                             
238000           IF W-IDARTNR > 0      AND                                      
238100              W-ADLAGOMR-TOM IS NUMERIC                                   
238200             PERFORM IMS-GHU-WDT101-ADLAGTOM                              
238300           ELSE                                                           
238400             IF (W-IDARTNR > 0 AND                                        
238500                (W-KDSTAPF = 'R' OR 'V' OR 'L' OR 'E'))                   
238600               PERFORM IMS-GHU-WDT101-STATUS                              
238700             ELSE                                                         
238800               IF W-IDARTNR > 0                                           
238900                 PERFORM IMS-GHU-WDT101-1                                 
239000               END-IF                                                     
239100             END-IF                                                       
239200           END-IF                                                         
239300         END-IF                                                           
239400       END-IF                                                             
239500     END-IF                                                               
239600     .                                                                    
239700     EJECT                                                                
239800 FB-READ-LINEDATA SECTION.                                                
239900     PERFORM IMS-GHU-WDT101                                               
240000     .                                                                    
240100     EJECT                                                                
240200 XX-MOVE-WDT101-TO-MOD SECTION.                                           
240300     MOVE PF-IDARTNR          TO MOD-IDARTNR     (INDX)                   
240400                                 W-IDARTNR                                
240500     MOVE ZERO                TO MOD-KVREFBER    (INDX)                   
240600                                                                          
240700     PERFORM IMS-GU-WDK611                                                
240800     IF SEGMENT-FOUND                                                     
240900       MOVE CLAG-KVQPACK-3    TO MOD-KVQPACK-3   (INDX)                   
241000       IF PF-ADLAGOMR-TOM = 10 OR 91                                      
241100         MOVE CLAG-KVREFBER-PLOCK TO MOD-KVREFBER(INDX)                   
241200       END-IF                                                             
241300     END-IF                                                               
241400     MOVE PF-ADLAGOMR-FOM     TO MOD-ADLAGOMR-FOM(INDX)                   
241500     MOVE PF-ADGANG-FOM       TO MOD-ADGANG-FOM  (INDX)                   
241600     MOVE PF-ADPLATS-FOM      TO MOD-ADPLATS-FOM (INDX)                   
241700     MOVE PF-KVBEST           TO MOD-KVBEST      (INDX)                   
241800     MOVE PF-ADLAGOMR-TOM     TO MOD-ADLAGOMR-TOM(INDX)                   
241900     MOVE PF-ADGANG-TOM       TO MOD-ADGANG-TOM  (INDX)                   
242000     MOVE PF-ADPLATS-TOM      TO MOD-ADPLATS-TOM (INDX)                   
242100     MOVE PF-KVBEST-ANDR      TO MOD-KVBEST-ANDR (INDX)                   
242200     MOVE PF-IDUSER           TO MOD-IDUSER      (INDX)                   
242300     MOVE PF-KDSTAPF          TO MOD-KDSTAPF     (INDX)                   
242400     MOVE PF-TIORDTIME        TO MOD-TIORDTIME   (INDX)                   
242500     IF PF-KDPRIO-PF =  1                                                 
242600       MOVE 'J'               TO MOD-KDPRIO      (INDX)                   
242700     ELSE                                                                 
242800       IF PF-KDPRIO-PF =  2                                               
242900         MOVE 'N'             TO MOD-KDPRIO      (INDX)                   
243000       END-IF                                                             
243100     END-IF                                                               
243200     .                                                                    
243300     EJECT                                                                
243400 XX-CLOSE-ERASE-FIELD SECTION.                                            
243500     MOVE MFS-CLOSE-FIELD TO MOD-KDCMDVAL-ATTR (INDX)                     
243600     MOVE MFS-ERASE-FIELD TO MOD-IDARTNR     (INDX)                       
243700                             MOD-ADLAGOMR-FOM(INDX)                       
243800                             MOD-ADGANG-FOM  (INDX)                       
243900                             MOD-ADPLATS-FOM (INDX)                       
244000                             MOD-KVBEST      (INDX)                       
244100                             MOD-ADLAGOMR-TOM(INDX)                       
244200                             MOD-ADGANG-TOM  (INDX)                       
244300                             MOD-ADPLATS-TOM (INDX)                       
244400                             MOD-KVBEST-ANDR (INDX)                       
244500                             MOD-IDUSER      (INDX)                       
244600                             MOD-KDSTAPF     (INDX)                       
244700                             MOD-TIORDTIME   (INDX)                       
244800     .                                                                    
244900     EJECT                                                                
245000 G-CHECK-INPUT SECTION.                                                   
245100     MOVE YES  TO INDATA-SW                                               
245200     IF  (MID-KDCMDVAL (1) = ALL '+' OR SPACE)                            
245300     AND (MID-KDCMDVAL (2) = ALL '+' OR SPACE)                            
245400     AND (MID-KDCMDVAL (3) = ALL '+' OR SPACE)                            
245500     AND (MID-KDCMDVAL (4) = ALL '+' OR SPACE)                            
245600     AND (MID-KDCMDVAL (5) = ALL '+' OR SPACE)                            
245700     AND (MID-KDCMDVAL (6) = ALL '+' OR SPACE)                            
245800     AND (MID-KDCMDVAL (7) = ALL '+' OR SPACE)                            
245900     AND (MID-KDCMDVAL (8) = ALL '+' OR SPACE)                            
246000     AND (MID-KDCMDVAL (9) = ALL '+' OR SPACE)                            
246100     AND (MID-KDCMDVAL (10) = ALL '+' OR SPACE)                           
246200     AND (MID-KDCMDVAL (11) = ALL '+' OR SPACE)                           
246300     AND (MID-KDCMDVAL (12) = ALL '+' OR SPACE)                           
246400     AND (MID-KDCMDVAL (13) = ALL '+' OR SPACE)                           
246500     AND (MID-ADLAGOMR-FOM-NEW  = ALL '+' OR SPACE)                       
246600     AND (MID-ADGANG-FOM-NEW    = ALL '+' OR SPACE)                       
246700     AND (MID-ADPLATS-FOM-NEW   = ALL '+' OR SPACE)                       
246800     AND (MID-KVBEST-ANDR-NEW   = ALL '+' OR SPACE)                       
246900                                                                          
247000       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
247100       CALL WMEDKONV USING MED-WMEDAREA                                   
247200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
247300       MOVE NOO TO INDATA-SW                                              
247400     ELSE                                                                 
247500        MOVE +1 TO INDX                                                   
247600        PERFORM UNTIL INDX > MAX-INDX                                     
247700                                                                          
247800          MOVE MID-KDCMDVAL (INDX) TO MOD-KDCMDVAL (INDX)                 
247900                                                                          
248000          IF MID-KDCMDVAL (INDX) NOT = ALL '+'                            
248100          AND MID-KDCMDVAL (INDX) NOT = SPACE                             
248200             IF  MID-KDCMDVAL (INDX) NOT = 'B'                            
248300             AND MID-KDCMDVAL (INDX) NOT = 'D'                            
248400             AND MID-KDCMDVAL (INDX) NOT = 'V'                            
248500             AND MID-KDCMDVAL (INDX) NOT = 'L'                            
248600             AND MID-KDCMDVAL (INDX) NOT = 'C'                            
248700             AND MID-KDCMDVAL (INDX) NOT = 'A'                            
248800                MOVE MFS-ALPHA-FIELD-WRONG                                
248900                              TO MOD-KDCMDVAL-ATTR (INDX)                 
249000                MOVE NOO TO INDATA-SW                                     
249100             ELSE                                                         
249200** GET PF-STATUS VALUES FROM WDT101 SEGMENTS                              
249300                 MOVE 11                   TO W-IDDC-WDT101               
249400                 MOVE MID-IDARTNR (INDX)   TO W-IDARTNR-WDT101            
249500                 MOVE MID-TIORDTIME (INDX) TO W-TIORDTIME-WDT101          
249600                 PERFORM IMS-GHU-WDT101                                   
249700                 IF SEGMENT-FOUND                                         
249800                    IF MID-KDCMDVAL (INDX) = 'B' OR 'D'                   
249900                       CONTINUE                                           
250000                    ELSE                                                  
250100                       IF MID-KDCMDVAL (INDX) = 'V'                       
250200                       AND (PF-KDSTAPF NOT = 'R' AND 'E')                 
250300                        IF W6A164-NOK                                     
250400                           MOVE MFS-ALPHA-FIELD-WRONG                     
250500                             TO MOD-KDCMDVAL-ATTR (INDX)                  
250600                           MOVE NOO TO INDATA-SW                          
250700                        END-IF                                            
250800                       END-IF                                             
250900                      IF MID-KDCMDVAL (INDX) = 'L'                        
251000                         AND (PF-KDSTAPF NOT = 'R' AND 'V')               
251100                              MOVE MFS-ALPHA-FIELD-WRONG                  
251200                                TO MOD-KDCMDVAL-ATTR (INDX)               
251300                          MOVE NOO TO INDATA-SW                           
251400                      END-IF                                              
251500                      IF MID-KDCMDVAL (INDX) = 'C'                        
251600                      AND (PF-KDSTAPF = 'A' OR 'L')                       
251700                          MOVE MFS-ALPHA-FIELD-WRONG                      
251800                            TO MOD-KDCMDVAL-ATTR (INDX)                   
251900                          MOVE NOO TO INDATA-SW                           
252000                      END-IF                                              
252100                      IF MID-KDCMDVAL (INDX) = 'A'                        
252200                      AND PF-KDSTAPF NOT = 'L'                            
252300                          MOVE MFS-ALPHA-FIELD-WRONG                      
252400                               TO MOD-KDCMDVAL-ATTR (INDX)                
252500                          MOVE NOO TO INDATA-SW                           
252600                      END-IF                                              
252700                                                                          
252800                      IF MID-KDCMDVAL (INDX) = 'C'                        
252900                      AND (PF-KDSTAPF NOT = 'A' OR 'L')                   
253000                          IF ((MID-ADLAGOMR-FOM-NEW                       
253100                                            = ALL '+' OR SPACES)          
253200                          AND (MID-ADGANG-FOM-NEW  = ALL '+'              
253300                                                     OR SPACES)           
253400                          AND (MID-ADPLATS-FOM-NEW =                      
253500                                             ALL '+' OR SPACES)           
253600                          AND (MID-KVBEST-ANDR-NEW =                      
253700                                             ALL '+' OR SPACES))          
253800                              MOVE MFS-ALPHA-FIELD-WRONG                  
253900                                   TO MOD-KDCMDVAL-ATTR (INDX)            
254000                              MOVE NOO TO INDATA-SW                       
254100                          END-IF                                          
254200                      END-IF                                              
254300                      IF INDATA-OK                                        
254400                         MOVE MFS-ALPHA-FIELD-OK                          
254500                                   TO MOD-KDCMDVAL-ATTR (INDX)            
254600                      END-IF                                              
254700                    END-IF                                                
254800                 END-IF                                                   
254900             END-IF                                                       
255000          END-IF                                                          
255100       ADD +1 TO INDX                                                     
255200       END-PERFORM                                                        
255300       IF INDATA-WRONG                                                    
255400          MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                       
255500          CALL WMEDKONV USING MED-WMEDAREA                                
255600          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
255700          PERFORM MFS-DONT-TOUCH-FIELD-IN                                 
255800          PERFORM MFS-DONT-TOUCH-FIELD-OUT                                
255900       END-IF                                                             
256000     END-IF                                                               
256100     .                                                                    
256200     EJECT                                                                
256300 S11-CHECK-INPUT SECTION.                                                 
256400     MOVE YES TO JUMP-CHK-SW                                              
256500     MOVE NOO TO JUMP-SW                                                  
256600                                                                          
256700     MOVE +1 TO INDX                                                      
256800     MOVE ZERO TO W-CMD-X W-CMD-C                                         
256900     MOVE +0   TO W-INDX                                                  
257000     PERFORM UNTIL INDX > MAX-INDX                                        
257100      IF MID-KDCMDVAL (INDX) = ALL 'X'                                    
257200        ADD 1  TO W-CMD-X                                                 
257300        MOVE INDX TO W-INDX                                               
257400      END-IF                                                              
257500      IF MID-KDCMDVAL (INDX) = ALL 'C'                                    
257600        ADD 1  TO W-CMD-C                                                 
257700        MOVE INDX TO W-INDX                                               
257800      END-IF                                                              
257900      ADD +1 TO INDX                                                      
258000     END-PERFORM                                                          
258100                                                                          
258200     IF W-CMD-X > 1                                                       
258300       MOVE NOO                    TO JUMP-CHK-SW                         
258400       MOVE MFS-ADD-READ-HILIGHT-FIELD                                    
258500                                   TO MOD-KDCMDVAL-ATTR (W-INDX)          
258600       CALL WMEDKONV USING MED-WMEDAREA                                   
258700       MOVE MED-MFSINF TO MOD-TEMFSFEL                                    
258800     ELSE                                                                 
258900       IF W-CMD-X = 1                                                     
259000          MOVE YES TO JUMP-CHK-SW                                         
259100          MOVE YES TO JUMP-SW                                             
259200       END-IF                                                             
259300     END-IF                                                               
259400                                                                          
259500     IF W-CMD-C > 1                                                       
259600       MOVE NOO                    TO CHK-CMD-C-SW                        
259700       MOVE MFS-ADD-READ-HILIGHT-FIELD                                    
259800                                   TO MOD-KDCMDVAL-ATTR (W-INDX)          
259900       CALL WMEDKONV USING MED-WMEDAREA                                   
260000       MOVE MED-MFSINF TO MOD-TEMFSFEL                                    
260100     END-IF                                                               
260200                                                                          
260300     IF JUMP-CHK-WRONG                                                    
260400       MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                          
260500       CALL WMEDKONV USING MED-WMEDAREA                                   
260600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
260700       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
260800       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
260900     END-IF                                                               
261000                                                                          
261100     IF CMD-C-CHK-WRONG                                                   
261200       MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                          
261300       CALL WMEDKONV USING MED-WMEDAREA                                   
261400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
261500       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
261600       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
261700     END-IF                                                               
261800     .                                                                    
261900     EJECT                                                                
262000 S12-CALL-PRINTER SECTION.                                                
262100     MOVE 001                TO PRT-KDCALL                                
262200     MOVE '6M'               TO WS-IDPRT1                                 
262300     MOVE MID-IDPRTLST       TO WS-IDPRT2                                 
262400     MOVE SPACE              TO WS-IDPRT3                                 
262500     MOVE WS-IDPRT           TO PRT-IDPRTLST                              
262600     MOVE SPACE              TO PRT-IDLTERM                               
262700                                                                          
262800     CALL W006PRT USING PRT-W006PRT                                       
262900                                                                          
263000     IF PRT-KDSVAR = 'R'                                                  
263100       PERFORM S12A-SEND-PRINTING                                         
263200     ELSE                                                                 
263300       MOVE NOO                  TO INDATA-SW                             
263400       IF MID-IDPRTLST > SPACES                                           
263500         MOVE ERR-PRINT-NOT-FOUND  TO MED-IDMFSINF                        
263600         CALL WMEDKONV USING MED-WMEDAREA                                 
263700         MOVE MED-TEMFSINF TO MOD-TEMFSFEL                                
263800       ELSE                                                               
263900         MOVE NO-PRINT-ENTER TO MOD-TEMFSFEL                              
264000       END-IF                                                             
264100       MOVE MFS-ALPHA-FIELD-WRONG TO MOD-IDPRTLST-ATTR                    
264200       MOVE MID-IDPRTLST TO MOD-IDPRTLST                                  
264300     END-IF                                                               
264400     .                                                                    
264500     EJECT                                                                
264600 S12A-SEND-PRINTING SECTION.                                              
264700     MOVE PRT-IDLTERM           TO URV-IDPRINTER                          
264800     MOVE MOD-ADLAGOMR-FOM-UT   TO URV-ADLAGOMR-FOM                       
264900     MOVE MOD-ADGANG-FOM-UT     TO URV-ADGANG-FOM                         
265000     MOVE MOD-ADLAGOMR-TOM-UT   TO URV-ADLAGOMR-TOM                       
265100     MOVE MOD-ADGANG-TOM-UT     TO URV-ADGANG-TOM                         
265200     MOVE MOD-KDSTAPF-UT        TO URV-KDSTAPF                            
265300     MOVE MOD-IDARTNR-UT        TO URV-IDARTNR                            
265400     MOVE MOD-IDUSER-UT         TO URV-IDUSER                             
265500     MOVE MOD-KDPRIO-PF-UT      TO URV-KDPRIO-PF                          
265600     MOVE '6164'                TO MSGSOP-IDTRANS                         
265700     MOVE '1'                   TO MSGSOP-KDMFSFOR                        
265800     MOVE 'W613S3'              TO MSGSOP-IDPROCESS                       
265900     MOVE 'O'                   TO MSGSOP-KDSOPFUNK                       
266000     STRING 'URVAL(' WS-URVAL ')PRT('                                     
266100            WS-PRINTER ')'                                                
266200            DELIMITED BY SIZE INTO MSGSOP-TESYMBV                         
266300                                                                          
266400     PERFORM IMS-INSERT-ALTMSG                                            
266500     MOVE INF-PRINT-BEGAERD TO MED-IDMFSINF                               
266600     CALL WMEDKONV USING MED-WMEDAREA                                     
266700     MOVE MED-TEMFSINF      TO MOD-TEMFSFEL                               
266800     .                                                                    
266900     EJECT                                                                
267000 S13-REMOVE-SPACES SECTION.                                               
267100     IF MID-KVBEST-ANDR-NEW > SPACES                                      
267200     AND NOT = ALL '+'                                                    
267300       MOVE ZERO TO W-TALLY                                               
267400                    W-LEN                                                 
267500                    W-KVBEST-ANDR-NEW-N                                   
267600       MOVE SPACES TO W-KVBEST-ANDR-NEW                                   
267700       MOVE MID-KVBEST-ANDR-NEW TO W-KVBEST-ANDR-NEW                      
267800       INSPECT FUNCTION REVERSE(W-KVBEST-ANDR-NEW) TALLYING               
267900               W-TALLY FOR LEADING SPACES                                 
268000       COMPUTE W-LEN = (LENGTH OF W-KVBEST-ANDR-NEW ) -                   
268100                       W-TALLY                                            
268200       MOVE W-KVBEST-ANDR-NEW(1:W-LEN) TO                                 
268300            W-KVBEST-ANDR-NEW-N (W-TALLY + 1:W-LEN)                       
268400       MOVE W-KVBEST-ANDR-NEW-N TO PF-KVBEST-ANDR                         
268500     END-IF                                                               
268600                                                                          
268700     IF MID-ADLAGOMR-FOM-NEW > SPACES                                     
268800     AND NOT = ALL '+'                                                    
268900       MOVE ZERO TO W-TALLY                                               
269000                    W-LEN                                                 
269100                    W-ADLAGOMR-FOM-NEW-N                                  
269200       MOVE SPACES TO W-ADLAGOMR-FOM-NEW                                  
269300       MOVE MID-ADLAGOMR-FOM-NEW TO W-ADLAGOMR-FOM-NEW                    
269400       INSPECT FUNCTION REVERSE(W-ADLAGOMR-FOM-NEW)                       
269500               TALLYING W-TALLY FOR LEADING SPACES                        
269600       COMPUTE W-LEN = (LENGTH OF W-ADLAGOMR-FOM-NEW ) -                  
269700                       W-TALLY                                            
269800       MOVE W-ADLAGOMR-FOM-NEW (1:W-LEN) TO                               
269900            W-ADLAGOMR-FOM-NEW-N (W-TALLY + 1:W-LEN)                      
270000       MOVE W-ADLAGOMR-FOM-NEW-N TO PF-ADLAGOMR-FOM                       
270100       MOVE 'R' TO PF-KDSTAPF                                             
270200     END-IF                                                               
270300                                                                          
270400     IF MID-ADGANG-FOM-NEW > SPACES                                       
270500     AND NOT = ALL '+'                                                    
270600       MOVE ZERO TO W-TALLY                                               
270700                    W-LEN                                                 
270800                    W-ADGANG-FOM-NEW-N                                    
270900       MOVE SPACES TO W-ADGANG-FOM-NEW                                    
271000       MOVE MID-ADGANG-FOM-NEW TO W-ADGANG-FOM-NEW                        
271100       INSPECT FUNCTION REVERSE(W-ADGANG-FOM-NEW)                         
271200               TALLYING W-TALLY FOR LEADING SPACES                        
271300       COMPUTE W-LEN = (LENGTH OF W-ADGANG-FOM-NEW ) -                    
271400                       W-TALLY                                            
271500       MOVE W-ADGANG-FOM-NEW (1:W-LEN) TO                                 
271600            W-ADGANG-FOM-NEW-N (W-TALLY + 1:W-LEN)                        
271700       MOVE W-ADGANG-FOM-NEW-N TO PF-ADGANG-FOM                           
271800       MOVE 'R' TO PF-KDSTAPF                                             
271900     END-IF                                                               
272000                                                                          
272100     IF MID-ADPLATS-FOM-NEW > SPACES                                      
272200     AND NOT = ALL '+'                                                    
272300       MOVE ZERO TO W-TALLY                                               
272400                    W-LEN                                                 
272500                    W-ADPLATS-FOM-NEW-N                                   
272600       MOVE SPACES TO W-ADPLATS-FOM-NEW                                   
272700       MOVE MID-ADPLATS-FOM-NEW TO W-ADPLATS-FOM-NEW                      
272800       INSPECT FUNCTION REVERSE(W-ADPLATS-FOM-NEW)                        
272900               TALLYING W-TALLY FOR LEADING SPACES                        
273000       COMPUTE W-LEN = (LENGTH OF W-ADPLATS-FOM-NEW ) -                   
273100                       W-TALLY                                            
273200       MOVE W-ADPLATS-FOM-NEW (1:W-LEN) TO                                
273300            W-ADPLATS-FOM-NEW-N (W-TALLY + 1:W-LEN)                       
273400       MOVE W-ADPLATS-FOM-NEW-N TO PF-ADPLATS-FOM                         
273500       MOVE 'R' TO PF-KDSTAPF                                             
273600     END-IF                                                               
273700     .                                                                    
273800 H-UPDATE SECTION.                                                        
273900     MOVE NOO TO UPDATE-SW                                                
274000     MOVE +1 TO INDX                                                      
274100     IF W6A164-OK                                                         
274200      MOVE +1 TO MAX-INDX                                                 
274300     END-IF                                                               
274400     PERFORM UNTIL INDX > MAX-INDX                                        
274500**     COMMON GET OF WDT101 FOR ALL POSSIBLE UPDATES                      
274600       MOVE 11                   TO W-IDDC-WDT101                         
274700       MOVE MID-IDARTNR (INDX)   TO W-IDARTNR-WDT101                      
274800       MOVE MID-TIORDTIME (INDX) TO W-TIORDTIME-WDT101                    
274900       PERFORM IMS-GHU-WDT101                                             
275000       IF SEGMENT-FOUND                                                   
275100         IF MID-KDCMDVAL (INDX) = 'B' OR 'D'                              
275200           PERFORM IMS-DLET-WDT101                                        
275300           MOVE YES TO UPDATE-SW                                          
275310           IF MID-ADLAGOMR-FOM (INDX) = '01'                              
                  AND PF-KDSTAPF = 'V'                                          
275311            MOVE MID-IDARTNR (INDX)   TO SYNQC-IDARTNR                    
                  INSPECT SYNQC-IDARTNR REPLACING ALL SPACE BY ZERO             
275312            MOVE MID-TIORDTIME (INDX) TO SYNQC-TIORDTIME                  
275313            MOVE 'RPL'                TO SYNQC-ORDERTYPE                  
275314            CALL W488ORCN USING  SYNQC-W488ORCN SYNQ-PCB                  
275315                               SYNQ-ATAB-PCB WDQ3-PCB                     
275320           END-IF                                                         
275400         END-IF                                                           
275500                                                                          
275600         IF MID-KDCMDVAL (INDX) = 'V'                                     
275700         AND (PF-KDSTAPF = 'R' OR W6A164-OK OR PF-KDSTAPF = 'E')          
275800          IF W6A164-OK                                                    
275900           IF MID-KDORDSTA-SYNQ ='COMPLETED'                              
276000           MOVE 'L' TO PF-KDSTAPF                                         
276100           MOVE MSGI-IDUSER TO PF-IDUSER                                  
276200           END-IF                                                         
276300           IF MID-KDORDSTA-SYNQ ='CANCELLED'                              
276400           MOVE 'E' TO PF-KDSTAPF                                         
276500           MOVE MSGI-IDUSER TO PF-IDUSER                                  
276600           END-IF                                                         
276700          ELSE                                                            
276800           MOVE 'V' TO PF-KDSTAPF                                         
276900           MOVE MSGI-IDUSER TO PF-IDUSER                                  
277000           IF MID-ADLAGOMR-FOM (INDX) = '01'                              
277200           MOVE MID-ADLAGOMR-TOM (INDX) TO SYNQ-ADLAGOMR-TOM              
277300           MOVE MID-ADGANG-TOM (INDX)   TO SYNQ-ADGANG-TOM                
277400           MOVE MID-IDARTNR (INDX)   TO SYNQ-IDARTNR                      
277500           INSPECT SYNQ-IDARTNR REPLACING ALL SPACE BY ZERO               
277600           IF MID-KVBEST-ANDR (INDX) > 0                                  
277700            MOVE MID-KVBEST-ANDR (INDX) TO SYNQ-KVBEST                    
277800           ELSE                                                           
277900            MOVE MID-KVBEST (INDX) TO SYNQ-KVBEST                         
278000           END-IF                                                         
278100           MOVE MID-TIORDTIME (INDX) TO SYNQ-TIORDTIME                    
278200           MOVE 'RPL'             TO SYNQ-ORDERTYPE                       
278300           CALL W488ORCR USING  SYNQ-W488ORCR SYNQ-PCB                    
278400                              SYNQ-ATAB-PCB WDQ3-PCB                      
278500           IF SYNQ-KDSVAR > SPACES                                        
278600            MOVE 'E' TO PF-KDSTAPF                                        
278700           END-IF                                                         
278800           END-IF                                                         
278900          END-IF                                                          
279000           PERFORM IMS-REPL-WDT101                                        
279100           MOVE YES TO UPDATE-SW                                          
279200         END-IF                                                           
279300                                                                          
279400         IF MID-KDCMDVAL (INDX) = 'L'                                     
279500         AND (PF-KDSTAPF = 'R' OR 'V')                                    
279600**         UPDATES OF WDT101 DATABASES                                    
279700           IF PF-KVBEST-ANDR = 0                                          
279800              MOVE PF-KVBEST       TO PF-KVBEST-ANDR                      
279900           END-IF                                                         
280000           MOVE 'L' TO PF-KDSTAPF                                         
280100           MOVE MSGI-IDUSER TO PF-IDUSER                                  
280200           MOVE TODAYS-DATE TO PF-TIHOTIME(1:6)                           
280300           MOVE TODAYS-TIME-HHMMSS TO PF-TIHOTIME(7:6)                    
280400           PERFORM IMS-REPL-WDT101                                        
280500           MOVE YES TO UPDATE-SW                                          
280600**         UPDATES OF WDD811 DATABASES                                    
280700           MOVE MID-ADLAGOMR-FOM (INDX) TO W-ADBUFFOM                     
280800           MOVE MID-ADGANG-FOM (INDX)   TO W-ADBUFGAN                     
280900           MOVE MID-ADPLATS-FOM (INDX)  TO W-ADBUFPL                      
281000           MOVE MID-IDARTNR (INDX)      TO W-IDARTNR                      
281100           IF W-ADBUFFOM = 1                                              
281200**           IF HIGH-BAY ADDR, DO NOT MAKE ANY DATABASE UPDATES           
281300             CONTINUE                                                     
281400           ELSE                                                           
281500             PERFORM IMS-GHU-WDD811                                       
281600             IF SEGMENT-FOUND                                             
281700               MOVE PF-KVBEST-ANDR       TO W-KVBEST-ANDR                 
281800               IF SALDO-KVBUFF-F <= W-KVBEST-ANDR                         
281900                 IF SALDO-ADBUFFOMR = 1 OR SALDO-DABUFPAF = 0             
282000                    OR SALDO-KVBUFF-OF > 0                                
282100                    MOVE ZERO TO SALDO-KVBUFF-F                           
282200                    MOVE ZERO TO SALDO-KVKOLLI-F                          
282300                    PERFORM IMS-REPL-WDD811                               
282400                 ELSE                                                     
282500                    PERFORM IMS-DLET-WDD811                               
282600                    PERFORM HA-UPDATE-WDJ9                                
282700                 END-IF                                                   
282800               ELSE                                                       
282900                 IF SALDO-KVKOLLI-F > 1                                   
283000                   SUBTRACT 1 FROM SALDO-KVKOLLI-F                        
283100                 END-IF                                                   
283200                 COMPUTE SALDO-KVBUFF-F = SALDO-KVBUFF-F                  
283300                                     - W-KVBEST-ANDR                      
283400                 PERFORM IMS-REPL-WDD811                                  
283500               END-IF                                                     
283600             END-IF                                                       
283700           END-IF                                                         
283800         END-IF                                                           
283900                                                                          
284000         IF MID-KDCMDVAL (INDX) = 'C'                                     
284100         AND (PF-KDSTAPF NOT = 'A' OR 'L')                                
284200           PERFORM S13-REMOVE-SPACES                                      
284300           MOVE MSGI-IDUSER TO PF-IDUSER                                  
284400           PERFORM IMS-REPL-WDT101                                        
284500           MOVE YES TO UPDATE-SW                                          
284600         END-IF                                                           
284700                                                                          
284800         IF MID-KDCMDVAL (INDX) = 'A'                                     
284900         AND PF-KDSTAPF = 'L'                                             
285000           MOVE 'A'                TO PF-KDSTAPF                          
285100           MOVE MSGI-IDUSER        TO PF-IDUSER                           
285200           MOVE TODAYS-DATE        TO PF-TIAVSL(1:6)                      
285300           MOVE TODAYS-TIME-HHMMSS TO PF-TIAVSL(7:6)                      
285400           PERFORM IMS-REPL-WDT101                                        
285500           MOVE YES                TO UPDATE-SW                           
285600         END-IF                                                           
285700       END-IF                                                             
285800       ADD +1 TO INDX                                                     
285900     END-PERFORM                                                          
286000                                                                          
286100     IF UPDATE-SUCCESS                                                    
286200       MOVE INF-UPDATE-DONE  TO MED-IDMFSINF                              
286300       CALL WMEDKONV USING MED-WMEDAREA                                   
286400       MOVE MED-TEMFSINF TO MOD-TEMFSINF                                  
286500       PERFORM MFS-ERASE-KDCMDVAL-FIELD-OUT                               
286600     END-IF                                                               
286700     .                                                                    
286800     EJECT                                                                
286900 HA-UPDATE-WDJ9 SECTION.                                                  
287000     PERFORM IMS-GU-WDJ901                                                
287100     IF SEGMENT-FOUND                                                     
287200       PERFORM IMS-GHNP-WDJ911                                            
287300       IF SEGMENT-FOUND                                                   
287400         PERFORM UNTIL SEGMENT-MISSING OR                                 
287500           (SALDO-ADBUFFOMR  = HIST-ADLAGOMR)                             
287600                                                     AND                  
287700           (SALDO-ADBUFFGANG = HIST-ADGANG)                               
287800                                                     AND                  
287900           (SALDO-ADBUFFPL   = HIST-ADPLATS)                              
288000                                                     AND                  
288100           (HIST-KDLOC = 'B')                                             
288200           PERFORM IMS-GHNP-WDJ911                                        
288300         END-PERFORM                                                      
288400         IF SEGMENT-FOUND AND                                             
288500           (SALDO-ADBUFFOMR  = HIST-ADLAGOMR)                             
288600                                                     AND                  
288700           (SALDO-ADBUFFGANG = HIST-ADGANG)                               
288800                                                     AND                  
288900           (SALDO-ADBUFFPL   = HIST-ADPLATS)                              
289000                                                     AND                  
289100           (HIST-KDLOC = 'B')                                             
289200           MOVE FUNCTION CURRENT-DATE(1:8) TO                             
289300                                    HIST-DASTODAT                         
289400           MOVE MSGI-IDUSER TO HIST-IDUSER-STO                            
289500           PERFORM IMS-REPL-WDJ911                                        
289600         END-IF                                                           
289700       END-IF                                                             
289800     END-IF                                                               
289900     .                                                                    
290000     EJECT                                                                
290100 I-JUMP-6162 SECTION.                                                     
290200     MOVE ALL '+'               TO 6162-MID-W6I16201                      
290300     MOVE MID-IDARTNR (W-INDX)  TO 6162-MID-IDARTNR-IN                    
290400     COMPUTE MSG-KVLL = LENGTH OF MOD-W6O16401 + 17                       
290500     PERFORM IMS-INSERT-ALT2-MSG                                          
290600     MOVE NOO                 TO ALLT-SW                                  
290700     .                                                                    
290800     EJECT                                                                
290900 MFS-ERASE-FIELD-OUT SECTION.                                             
291000     MOVE MFS-ERASE-FIELD TO MOD-ADLAGOMR-FOM-UT                          
291100                             MOD-ADLAGOMR-TOM-UT                          
291200                             MOD-KDSTAPF-UT                               
291300                             MOD-ADGANG-FOM-UT                            
291400                             MOD-ADGANG-TOM-UT                            
291500                             MOD-IDARTNR-UT                               
291600                             MOD-IDUSER-UT                                
291700                             MOD-KDPRIO-PF-UT                             
291800     .                                                                    
291900     SKIP3                                                                
292000 MFS-ERASE-KDCMDVAL-FIELD-OUT SECTION.                                    
292100     MOVE +1 TO INDX                                                      
292200     PERFORM UNTIL INDX > MAX-INDX                                        
292300       MOVE MFS-ERASE-FIELD TO MOD-KDCMDVAL (INDX)                        
292400       ADD +1 TO INDX                                                     
292500     END-PERFORM                                                          
292600     .                                                                    
292700     SKIP3                                                                
292800 MFS-ERASE-FIELD-IN SECTION.                                              
292900     MOVE MFS-ERASE-FIELD TO MID-ADLAGOMR-FOM-IN                          
293000                             MID-ADGANG-FOM-IN                            
293100                             MID-ADLAGOMR-TOM-IN                          
293200                             MID-ADGANG-TOM-IN                            
293300                             MID-KDSTAPF-IN                               
293400                             MID-IDARTNR-IN                               
293500                             MID-IDUSER-IN                                
293600                             MID-KDPRIO-PF-IN                             
293700     .                                                                    
293800     EJECT                                                                
293900 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
294000     MOVE +1 TO INDX                                                      
294100     PERFORM UNTIL INDX > MAX-INDX                                        
294200       PERFORM MFS-DONT-TOUCH-LINE-FIELD-OUT                              
294300       ADD +1 TO INDX                                                     
294400     END-PERFORM                                                          
294500     .                                                                    
294600     SKIP2                                                                
294700 MFS-DONT-TOUCH-LINE-FIELD-OUT  SECTION.                                  
294800*    --- OUTDATA FIELD ON SCROLL KEYS                                     
294900     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDCMDVAL    (INDX)                
295000                                    MOD-IDARTNR     (INDX)                
295100                                    MOD-ADLAGOMR-FOM(INDX)                
295200                                    MOD-ADGANG-FOM  (INDX)                
295300                                    MOD-ADPLATS-FOM (INDX)                
295400                                    MOD-KVBEST      (INDX)                
295500                                    MOD-ADLAGOMR-FOM(INDX)                
295600                                    MOD-ADLAGOMR-TOM(INDX)                
295700                                    MOD-ADGANG-TOM  (INDX)                
295800                                    MOD-ADPLATS-TOM (INDX)                
295900                                    MOD-KVBEST-ANDR (INDX)                
296000                                    MOD-IDUSER      (INDX)                
296100                                    MOD-KDSTAPF     (INDX)                
296200                                    MOD-KDPRIO      (INDX)                
296300                                    MOD-KVQPACK-3   (INDX)                
296400                                    MOD-KVREFBER    (INDX)                
296500                                    MOD-TIORDTIME   (INDX)                
296600                                    MOD-ADLAGOMR-FOM-N                    
296700                                    MOD-ADGANG-FOM-N                      
296800                                    MOD-ADPLATS-FOM-N                     
296900                                    MOD-KVBEST-ANDR-N                     
297000     .                                                                    
297100     SKIP3                                                                
297200 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
297300     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-ADLAGOMR-FOM-IN                   
297400                                    MOD-ADGANG-FOM-IN                     
297500                                    MOD-ADLAGOMR-TOM-IN                   
297600                                    MOD-ADGANG-TOM-IN                     
297700                                    MOD-KDSTAPF-IN                        
297800                                    MOD-IDARTNR-IN                        
297900                                    MOD-IDUSER-IN                         
298000                                    MOD-KDPRIO-PF-IN                      
298100     .                                                                    
298200     EJECT                                                                
298300** IMS SECTION BEGINS                                                     
298400                                                                          
298500 IMS-INSERT-ALTMSG SECTION.                                               
298600     MOVE SPACE TO GOOD-STATUSCODES                                       
298700     CALL CBLTDLI USING PURG ALT1-PCB PROG-TO-PROG-SW1                    
298800     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
298900     PERFORM IMS-STATUSCHECK                                              
299000     .                                                                    
299100     EJECT                                                                
299200 IMS-GET-MSG SECTION.                                                     
299300     MOVE '  QC' TO GOOD-STATUSCODES                                      
299400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
299500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
299600     PERFORM IMS-STATUSCHECK                                              
299700     .                                                                    
299800     SKIP3                                                                
299900 IMS-INSERT-ALT2-MSG SECTION.                                             
300000     MOVE SPACE TO GOOD-STATUSCODES                                       
300100     CALL CBLTDLI USING ISRT ALT2-PCB W-PROG-TO-PROG-SW2                  
300200     MOVE ALT2-STATUS-CODE TO STATUS-WS                                   
300300     PERFORM IMS-STATUSCHECK                                              
300400     .                                                                    
300500     EJECT                                                                
300600 IMS-INSERT-MSG SECTION.                                                  
300700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
300800     MOVE SPACE TO GOOD-STATUSCODES                                       
300900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
301000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
301100     PERFORM IMS-STATUSCHECK                                              
301200     .                                                                    
301300     EJECT                                                                
301400 IMS-GHU-WDT101 SECTION.                                                  
301500     STRING 'WDT101  (WDT101KY =' W-WDT101KY-X ')'                        
301600          DELIMITED BY SIZE INTO SSA1                                     
301700     MOVE '  GE' TO GOOD-STATUSCODES                                      
301800     CALL CBLTDLI USING GHU WDT1-PCB DLI-IO-WDT101 SSA1                   
301900     MOVE WDT1-STATUS-CODE TO STATUS-WS                                   
302000     PERFORM IMS-STATUSCHECK                                              
302100     .                                                                    
302200     SKIP3                                                                
302300 IMS-GHU-WDT101-1 SECTION.                                                
302400     STRING 'WDT101  (WDT101KY>=' W-WDT101KY-MIN-X                        
302500                    '&WDT101KY<=' W-WDT101KY-MAX-X ')'                    
302600          DELIMITED BY SIZE INTO SSA1                                     
302700     MOVE '  GE' TO GOOD-STATUSCODES                                      
302800     CALL CBLTDLI USING GHU WDT1-PCB DLI-IO-WDT101 SSA1                   
302900     MOVE WDT1-STATUS-CODE TO STATUS-WS                                   
303000     PERFORM IMS-STATUSCHECK                                              
303100     .                                                                    
303200     EJECT                                                                
303300 IMS-GU-WDT101 SECTION.                                                   
303400     STRING 'WDT101  (WDT101KY>=' W-WDT101KY-MIN-X                        
303500                    '&WDT101KY<=' W-WDT101KY-MAX-X ')'                    
303600          DELIMITED BY SIZE INTO SSA1                                     
303700     MOVE '  GE' TO GOOD-STATUSCODES                                      
303800     CALL CBLTDLI USING GU WDT1-PCB DLI-IO-WDT101 SSA1                    
303900     MOVE WDT1-STATUS-CODE TO STATUS-WS                                   
304000     PERFORM IMS-STATUSCHECK                                              
304100     .                                                                    
304200     EJECT                                                                
304300 IMS-GN-WDT101 SECTION.                                                   
304400     STRING 'WDT101  (WDT101KY>=' W-WDT101KY-MIN-X                        
304500                    '&WDT101KY<=' W-WDT101KY-MAX-X ')'                    
304600          DELIMITED BY SIZE INTO SSA1                                     
304700     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
304800     CALL CBLTDLI USING GN WDT1-PCB DLI-IO-WDT101 SSA1                    
304900     MOVE WDT1-STATUS-CODE TO STATUS-WS                                   
305000     PERFORM IMS-STATUSCHECK                                              
305100     .                                                                    
305200     SKIP3                                                                
305300 IMS-GHU-WDT101-STATUS SECTION.                                           
305400     STRING 'WDT101  (WDT101KY>=' W-WDT101KY-MIN-X                        
305500                    '&WDT101KY<=' W-WDT101KY-MAX-X                        
305600                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
305700          DELIMITED BY SIZE INTO SSA1                                     
305800     MOVE '  GE' TO GOOD-STATUSCODES                                      
305900     CALL CBLTDLI USING GU WDT1-PCB DLI-IO-WDT101 SSA1                    
306000     MOVE WDT1-STATUS-CODE TO STATUS-WS                                   
306100     PERFORM IMS-STATUSCHECK                                              
306200     .                                                                    
306300     EJECT                                                                
306400 IMS-GN-WDT101-STATUS SECTION.                                            
306500     STRING 'WDT101  (WDT101KY>=' W-WDT101KY-MIN-X                        
306600                    '&WDT101KY<=' W-WDT101KY-MAX-X                        
306700                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
306800          DELIMITED BY SIZE INTO SSA1                                     
306900     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
307000     CALL CBLTDLI USING GN WDT1-PCB DLI-IO-WDT101 SSA1                    
307100     MOVE WDT1-STATUS-CODE TO STATUS-WS                                   
307200     PERFORM IMS-STATUSCHECK                                              
307300     .                                                                    
307400     SKIP3                                                                
307500 IMS-GHU-WDT101-ADLAGFOM SECTION.                                         
307600     STRING 'WDT101  (WDT101KY>=' W-WDT101KY-MIN-X                        
307700                    '&WDT101KY<=' W-WDT101KY-MAX-X                        
307800                    '&ADLAGFOM =' W-ADLAGOMR-FOM-X ')'                    
307900          DELIMITED BY SIZE INTO SSA1                                     
308000     MOVE '  GE' TO GOOD-STATUSCODES                                      
308100     CALL CBLTDLI USING GHU WDT1-PCB DLI-IO-WDT101 SSA1                   
308200     MOVE WDT1-STATUS-CODE TO STATUS-WS                                   
308300     PERFORM IMS-STATUSCHECK                                              
308400     .                                                                    
308500     EJECT                                                                
308600 IMS-GN-WDT101-ADLAGFOM SECTION.                                          
308700     STRING 'WDT101  (WDT101KY>=' W-WDT101KY-MIN-X                        
308800                    '&WDT101KY<=' W-WDT101KY-MAX-X                        
308900                    '&ADLAGFOM =' W-ADLAGOMR-FOM-X ')'                    
309000          DELIMITED BY SIZE INTO SSA1                                     
309100     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
309200     CALL CBLTDLI USING GN WDT1-PCB DLI-IO-WDT101 SSA1                    
309300     MOVE WDT1-STATUS-CODE TO STATUS-WS                                   
309400     PERFORM IMS-STATUSCHECK                                              
309500     .                                                                    
309600     SKIP3                                                                
309700 IMS-GHU-WDT101-ADLAGFOM-STA SECTION.                                     
309800     STRING 'WDT101  (WDT101KY>=' W-WDT101KY-MIN-X                        
309900                    '&WDT101KY<=' W-WDT101KY-MAX-X                        
310000                    '&ADLAGFOM =' W-ADLAGOMR-FOM-X                        
310100                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
310200          DELIMITED BY SIZE INTO SSA1                                     
310300     MOVE '  GE' TO GOOD-STATUSCODES                                      
310400     CALL CBLTDLI USING GHU WDT1-PCB DLI-IO-WDT101 SSA1                   
310500     MOVE WDT1-STATUS-CODE TO STATUS-WS                                   
310600     PERFORM IMS-STATUSCHECK                                              
310700     .                                                                    
310800     EJECT                                                                
310900 IMS-GN-WDT101-ADLAGFOM-STA SECTION.                                      
311000     STRING 'WDT101  (WDT101KY>=' W-WDT101KY-MIN-X                        
311100                    '&WDT101KY<=' W-WDT101KY-MAX-X                        
311200                    '&ADLAGFOM =' W-ADLAGOMR-FOM-X                        
311300                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
311400          DELIMITED BY SIZE INTO SSA1                                     
311500     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
311600     CALL CBLTDLI USING GN WDT1-PCB DLI-IO-WDT101 SSA1                    
311700     MOVE WDT1-STATUS-CODE TO STATUS-WS                                   
311800     PERFORM IMS-STATUSCHECK                                              
311900     .                                                                    
312000     SKIP3                                                                
312100 IMS-GHU-WDT101-ADLAGTOM SECTION.                                         
312200     STRING 'WDT101  (WDT101KY>=' W-WDT101KY-MIN-X                        
312300                    '&WDT101KY<=' W-WDT101KY-MAX-X                        
312400                    '&ADLAGTOM =' W-ADLAGOMR-TOM-X ')'                    
312500          DELIMITED BY SIZE INTO SSA1                                     
312600     MOVE '  GE' TO GOOD-STATUSCODES                                      
312700     CALL CBLTDLI USING GHU WDT1-PCB DLI-IO-WDT101 SSA1                   
312800     MOVE WDT1-STATUS-CODE TO STATUS-WS                                   
312900     PERFORM IMS-STATUSCHECK                                              
313000     .                                                                    
313100     EJECT                                                                
313200 IMS-GN-WDT101-ADLAGTOM SECTION.                                          
313300     STRING 'WDT101  (WDT101KY>=' W-WDT101KY-MIN-X                        
313400                    '&WDT101KY<=' W-WDT101KY-MAX-X                        
313500                    '&ADLAGTOM =' W-ADLAGOMR-TOM-X ')'                    
313600          DELIMITED BY SIZE INTO SSA1                                     
313700     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
313800     CALL CBLTDLI USING GN WDT1-PCB DLI-IO-WDT101 SSA1                    
313900     MOVE WDT1-STATUS-CODE TO STATUS-WS                                   
314000     PERFORM IMS-STATUSCHECK                                              
314100     .                                                                    
314200     SKIP3                                                                
314300 IMS-GHU-WDT101-ADLAGTOM-STA SECTION.                                     
314400     STRING 'WDT101  (WDT101KY>=' W-WDT101KY-MIN-X                        
314500                    '&WDT101KY<=' W-WDT101KY-MAX-X                        
314600                    '&ADLAGTOM =' W-ADLAGOMR-TOM-X                        
314700                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
314800          DELIMITED BY SIZE INTO SSA1                                     
314900     MOVE '  GE' TO GOOD-STATUSCODES                                      
315000     CALL CBLTDLI USING GHU WDT1-PCB DLI-IO-WDT101 SSA1                   
315100     MOVE WDT1-STATUS-CODE TO STATUS-WS                                   
315200     PERFORM IMS-STATUSCHECK                                              
315300     .                                                                    
315400     EJECT                                                                
315500 IMS-GN-WDT101-ADLAGTOM-STA SECTION.                                      
315600     STRING 'WDT101  (WDT101KY>=' W-WDT101KY-MIN-X                        
315700                    '&WDT101KY<=' W-WDT101KY-MAX-X                        
315800                    '&ADLAGTOM =' W-ADLAGOMR-TOM-X                        
315900                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
316000          DELIMITED BY SIZE INTO SSA1                                     
316100     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
316200     CALL CBLTDLI USING GN WDT1-PCB DLI-IO-WDT101 SSA1                    
316300     MOVE WDT1-STATUS-CODE TO STATUS-WS                                   
316400     PERFORM IMS-STATUSCHECK                                              
316500     .                                                                    
316600     SKIP3                                                                
316700 IMS-GU-WDT1A1 SECTION.                                                   
316800     STRING 'WDT1A1  (WDT1A1KY>=' W-WDT1A1KY-MIN-X                        
316900                    '&WDT1A1KY<=' W-WDT1A1KY-MAX-X ')'                    
317000          DELIMITED BY SIZE INTO SSA1                                     
317100     MOVE '  GE' TO GOOD-STATUSCODES                                      
317200     CALL CBLTDLI USING GU WDT1A-PCB DLI-IO-WDT1A1 SSA1                   
317300     MOVE WDT1A-STATUS-CODE TO STATUS-WS                                  
317400     PERFORM IMS-STATUSCHECK                                              
317500     .                                                                    
317600     SKIP3                                                                
317700 IMS-GN-WDT1A1 SECTION.                                                   
317800     STRING 'WDT1A1  (WDT1A1KY>=' W-WDT1A1KY-MIN-X                        
317900                    '&WDT1A1KY<=' W-WDT1A1KY-MAX-X ')'                    
318000          DELIMITED BY SIZE INTO SSA1                                     
318100     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
318200     CALL CBLTDLI USING GN WDT1A-PCB DLI-IO-WDT1A1 SSA1                   
318300     MOVE WDT1A-STATUS-CODE TO STATUS-WS                                  
318400     PERFORM IMS-STATUSCHECK                                              
318500     .                                                                    
318600     SKIP3                                                                
318700 IMS-GU-WDT1A1-STATUS SECTION.                                            
318800     STRING 'WDT1A1  (WDT1A1KY>=' W-WDT1A1KY-MIN-X                        
318900                    '&WDT1A1KY<=' W-WDT1A1KY-MAX-X                        
319000                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
319100          DELIMITED BY SIZE INTO SSA1                                     
319200     MOVE '  GE' TO GOOD-STATUSCODES                                      
319300     CALL CBLTDLI USING GU WDT1A-PCB DLI-IO-WDT1A1 SSA1                   
319400     MOVE WDT1A-STATUS-CODE TO STATUS-WS                                  
319500     PERFORM IMS-STATUSCHECK                                              
319600     .                                                                    
319700     SKIP3                                                                
319800 IMS-GN-WDT1A1-STATUS SECTION.                                            
319900     STRING 'WDT1A1  (WDT1A1KY>=' W-WDT1A1KY-MIN-X                        
320000                    '&WDT1A1KY<=' W-WDT1A1KY-MAX-X                        
320100                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
320200          DELIMITED BY SIZE INTO SSA1                                     
320300     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
320400     CALL CBLTDLI USING GN WDT1A-PCB DLI-IO-WDT1A1 SSA1                   
320500     MOVE WDT1A-STATUS-CODE TO STATUS-WS                                  
320600     PERFORM IMS-STATUSCHECK                                              
320700     .                                                                    
320800     SKIP3                                                                
320900 IMS-GU-WDT1B1 SECTION.                                                   
321000     STRING 'WDT1B1  (WDT1B1KY>=' W-WDT1B1KY-MIN-X                        
321100                    '&WDT1B1KY<=' W-WDT1B1KY-MAX-X ')'                    
321200          DELIMITED BY SIZE INTO SSA1                                     
321300     MOVE '  GE' TO GOOD-STATUSCODES                                      
321400     CALL CBLTDLI USING GU WDT1B-PCB DLI-IO-WDT1B1 SSA1                   
321500     MOVE WDT1B-STATUS-CODE TO STATUS-WS                                  
321600     PERFORM IMS-STATUSCHECK                                              
321700     .                                                                    
321800     SKIP3                                                                
321900 IMS-GN-WDT1B1 SECTION.                                                   
322000     STRING 'WDT1B1  (WDT1B1KY>=' W-WDT1B1KY-MIN-X                        
322100                    '&WDT1B1KY<=' W-WDT1B1KY-MAX-X ')'                    
322200          DELIMITED BY SIZE INTO SSA1                                     
322300     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
322400     CALL CBLTDLI USING GN WDT1B-PCB DLI-IO-WDT1B1 SSA1                   
322500     MOVE WDT1B-STATUS-CODE TO STATUS-WS                                  
322600     PERFORM IMS-STATUSCHECK                                              
322700     .                                                                    
322800     SKIP3                                                                
322900 IMS-GU-WDT1B1-STATUS SECTION.                                            
323000     STRING 'WDT1B1  (WDT1B1KY>=' W-WDT1B1KY-MIN-X                        
323100                    '&WDT1B1KY<=' W-WDT1B1KY-MAX-X                        
323200                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
323300          DELIMITED BY SIZE INTO SSA1                                     
323400     MOVE '  GE' TO GOOD-STATUSCODES                                      
323500     CALL CBLTDLI USING GU WDT1B-PCB DLI-IO-WDT1B1 SSA1                   
323600     MOVE WDT1B-STATUS-CODE TO STATUS-WS                                  
323700     PERFORM IMS-STATUSCHECK                                              
323800     .                                                                    
323900     SKIP3                                                                
324000 IMS-GN-WDT1B1-STATUS SECTION.                                            
324100     STRING 'WDT1B1  (WDT1B1KY>=' W-WDT1B1KY-MIN-X                        
324200                    '&WDT1B1KY<=' W-WDT1B1KY-MAX-X                        
324300                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
324400          DELIMITED BY SIZE INTO SSA1                                     
324500     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
324600     CALL CBLTDLI USING GN WDT1B-PCB DLI-IO-WDT1B1 SSA1                   
324700     MOVE WDT1B-STATUS-CODE TO STATUS-WS                                  
324800     PERFORM IMS-STATUSCHECK                                              
324900     .                                                                    
325000     SKIP3                                                                
325100 IMS-GU-WDT1C1 SECTION.                                                   
325200     STRING 'WDT1C1  (WDT1C1KY>=' W-WDT1C1KY-MIN-X                        
325300                    '&WDT1C1KY<=' W-WDT1C1KY-MAX-X ')'                    
325400          DELIMITED BY SIZE INTO SSA1                                     
325500     MOVE '  GE' TO GOOD-STATUSCODES                                      
325600     CALL CBLTDLI USING GU WDT1C-PCB DLI-IO-WDT1C1 SSA1                   
325700     MOVE WDT1C-STATUS-CODE TO STATUS-WS                                  
325800     PERFORM IMS-STATUSCHECK                                              
325900     .                                                                    
326000     SKIP3                                                                
326100 IMS-GN-WDT1C1 SECTION.                                                   
326200     STRING 'WDT1C1  (WDT1C1KY>=' W-WDT1C1KY-MIN-X                        
326300                    '&WDT1C1KY<=' W-WDT1C1KY-MAX-X ')'                    
326400          DELIMITED BY SIZE INTO SSA1                                     
326500     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
326600     CALL CBLTDLI USING GN WDT1C-PCB DLI-IO-WDT1C1 SSA1                   
326700     MOVE WDT1C-STATUS-CODE TO STATUS-WS                                  
326800     PERFORM IMS-STATUSCHECK                                              
326900     .                                                                    
327000     SKIP3                                                                
327100 IMS-GU-WDT1C1-STATUS SECTION.                                            
327200     STRING 'WDT1C1  (WDT1C1KY>=' W-WDT1C1KY-MIN-X                        
327300                    '&WDT1C1KY<=' W-WDT1C1KY-MAX-X                        
327400                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
327500          DELIMITED BY SIZE INTO SSA1                                     
327600     MOVE '  GE' TO GOOD-STATUSCODES                                      
327700     CALL CBLTDLI USING GU WDT1C-PCB DLI-IO-WDT1C1 SSA1                   
327800     MOVE WDT1C-STATUS-CODE TO STATUS-WS                                  
327900     PERFORM IMS-STATUSCHECK                                              
328000     .                                                                    
328100     SKIP3                                                                
328200 IMS-GN-WDT1C1-STATUS SECTION.                                            
328300     STRING 'WDT1C1  (WDT1C1KY>=' W-WDT1C1KY-MIN-X                        
328400                    '&WDT1C1KY<=' W-WDT1C1KY-MAX-X                        
328500                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
328600          DELIMITED BY SIZE INTO SSA1                                     
328700     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
328800     CALL CBLTDLI USING GN WDT1C-PCB DLI-IO-WDT1C1 SSA1                   
328900     MOVE WDT1C-STATUS-CODE TO STATUS-WS                                  
329000     PERFORM IMS-STATUSCHECK                                              
329100     .                                                                    
329200     SKIP3                                                                
329300 IMS-GU-WDT1D1 SECTION.                                                   
329400     STRING 'WDT1D1  (WDT1D1KY>=' W-WDT1D1KY-MIN-X                        
329500                    '&WDT1D1KY<=' W-WDT1D1KY-MAX-X ')'                    
329600          DELIMITED BY SIZE INTO SSA1                                     
329700     MOVE '  GE' TO GOOD-STATUSCODES                                      
329800     CALL CBLTDLI USING GU WDT1D-PCB DLI-IO-WDT1D1 SSA1                   
329900     MOVE WDT1D-STATUS-CODE TO STATUS-WS                                  
330000     PERFORM IMS-STATUSCHECK                                              
330100     .                                                                    
330200     SKIP3                                                                
330300 IMS-GU-WDT1D1-STATUS SECTION.                                            
330400     STRING 'WDT1D1  (WDT1D1KY>=' W-WDT1D1KY-MIN-X                        
330500                    '&WDT1D1KY<=' W-WDT1D1KY-MAX-X                        
330600                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
330700          DELIMITED BY SIZE INTO SSA1                                     
330800     MOVE '  GE' TO GOOD-STATUSCODES                                      
330900     CALL CBLTDLI USING GU WDT1D-PCB DLI-IO-WDT1D1 SSA1                   
331000     MOVE WDT1D-STATUS-CODE TO STATUS-WS                                  
331100     PERFORM IMS-STATUSCHECK                                              
331200     .                                                                    
331300     SKIP3                                                                
331400 IMS-GN-WDT1D1 SECTION.                                                   
331500     STRING 'WDT1D1  (WDT1D1KY>=' W-WDT1D1KY-MIN-X                        
331600                    '&WDT1D1KY<=' W-WDT1D1KY-MAX-X ')'                    
331700          DELIMITED BY SIZE INTO SSA1                                     
331800     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
331900     CALL CBLTDLI USING GN WDT1D-PCB DLI-IO-WDT1D1 SSA1                   
332000     MOVE WDT1D-STATUS-CODE TO STATUS-WS                                  
332100     PERFORM IMS-STATUSCHECK                                              
332200     .                                                                    
332300     SKIP3                                                                
332400 IMS-GN-WDT1D1-STATUS SECTION.                                            
332500     STRING 'WDT1D1  (WDT1D1KY>=' W-WDT1D1KY-MIN-X                        
332600                    '&WDT1D1KY<=' W-WDT1D1KY-MAX-X                        
332700                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
332800          DELIMITED BY SIZE INTO SSA1                                     
332900     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
333000     CALL CBLTDLI USING GN WDT1D-PCB DLI-IO-WDT1D1 SSA1                   
333100     MOVE WDT1D-STATUS-CODE TO STATUS-WS                                  
333200     PERFORM IMS-STATUSCHECK                                              
333300     .                                                                    
333400     SKIP3                                                                
333500 IMS-GU-WDT1E1 SECTION.                                                   
333600     STRING 'WDT1E1  (WDT1E1KY>=' W-WDT1E1KY-MIN-X                        
333700                    '&WDT1E1KY<=' W-WDT1E1KY-MAX-X ')'                    
333800          DELIMITED BY SIZE INTO SSA1                                     
333900     MOVE '  GE' TO GOOD-STATUSCODES                                      
334000     CALL CBLTDLI USING GU WDT1E-PCB DLI-IO-WDT1E1 SSA1                   
334100     MOVE WDT1E-STATUS-CODE TO STATUS-WS                                  
334200     PERFORM IMS-STATUSCHECK                                              
334300     .                                                                    
334400     SKIP3                                                                
334500 IMS-GN-WDT1E1 SECTION.                                                   
334600     STRING 'WDT1E1  (WDT1E1KY>=' W-WDT1E1KY-MIN-X                        
334700                    '&WDT1E1KY<=' W-WDT1E1KY-MAX-X ')'                    
334800          DELIMITED BY SIZE INTO SSA1                                     
334900     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
335000     CALL CBLTDLI USING GN WDT1E-PCB DLI-IO-WDT1E1 SSA1                   
335100     MOVE WDT1E-STATUS-CODE TO STATUS-WS                                  
335200     PERFORM IMS-STATUSCHECK                                              
335300     .                                                                    
335400     SKIP3                                                                
335500 IMS-GU-WDT1E1-STATUS SECTION.                                            
335600     STRING 'WDT1E1  (WDT1E1KY>=' W-WDT1E1KY-MIN-X                        
335700                    '&WDT1E1KY<=' W-WDT1E1KY-MAX-X                        
335800                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
335900          DELIMITED BY SIZE INTO SSA1                                     
336000     MOVE '  GE' TO GOOD-STATUSCODES                                      
336100     CALL CBLTDLI USING GU WDT1E-PCB DLI-IO-WDT1E1 SSA1                   
336200     MOVE WDT1E-STATUS-CODE TO STATUS-WS                                  
336300     PERFORM IMS-STATUSCHECK                                              
336400     .                                                                    
336500     SKIP3                                                                
336600 IMS-GN-WDT1E1-STATUS SECTION.                                            
336700     STRING 'WDT1E1  (WDT1E1KY>=' W-WDT1E1KY-MIN-X                        
336800                    '&WDT1E1KY<=' W-WDT1E1KY-MAX-X                        
336900                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
337000          DELIMITED BY SIZE INTO SSA1                                     
337100     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
337200     CALL CBLTDLI USING GN WDT1E-PCB DLI-IO-WDT1E1 SSA1                   
337300     MOVE WDT1E-STATUS-CODE TO STATUS-WS                                  
337400     PERFORM IMS-STATUSCHECK                                              
337500     .                                                                    
337600     SKIP3                                                                
337700 IMS-GU-WDT1F1 SECTION.                                                   
337800     STRING 'WDT1F1  (WDT1F1KY>=' W-WDT1F1KY-MIN-X                        
337900                    '&WDT1F1KY<=' W-WDT1F1KY-MAX-X ')'                    
338000          DELIMITED BY SIZE INTO SSA1                                     
338100     MOVE '  GE' TO GOOD-STATUSCODES                                      
338200     CALL CBLTDLI USING GU WDT1F-PCB DLI-IO-WDT1F1 SSA1                   
338300     MOVE WDT1F-STATUS-CODE TO STATUS-WS                                  
338400     PERFORM IMS-STATUSCHECK                                              
338500     .                                                                    
338600     SKIP3                                                                
338700 IMS-GN-WDT1F1 SECTION.                                                   
338800     STRING 'WDT1F1  (WDT1F1KY>=' W-WDT1F1KY-MIN-X                        
338900                    '&WDT1F1KY<=' W-WDT1F1KY-MAX-X ')'                    
339000          DELIMITED BY SIZE INTO SSA1                                     
339100     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
339200     CALL CBLTDLI USING GN WDT1F-PCB DLI-IO-WDT1F1 SSA1                   
339300     MOVE WDT1F-STATUS-CODE TO STATUS-WS                                  
339400     PERFORM IMS-STATUSCHECK                                              
339500     .                                                                    
339600     SKIP3                                                                
339700 IMS-GU-WDT1F1-STATUS SECTION.                                            
339800     STRING 'WDT1F1  (WDT1F1KY>=' W-WDT1F1KY-MIN-X                        
339900                    '&WDT1F1KY<=' W-WDT1F1KY-MAX-X                        
340000                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
340100          DELIMITED BY SIZE INTO SSA1                                     
340200     MOVE '  GE' TO GOOD-STATUSCODES                                      
340300     CALL CBLTDLI USING GU WDT1F-PCB DLI-IO-WDT1F1 SSA1                   
340400     MOVE WDT1F-STATUS-CODE TO STATUS-WS                                  
340500     PERFORM IMS-STATUSCHECK                                              
340600     .                                                                    
340700     SKIP3                                                                
340800 IMS-GN-WDT1F1-STATUS SECTION.                                            
340900     STRING 'WDT1F1  (WDT1F1KY>=' W-WDT1F1KY-MIN-X                        
341000                    '&WDT1F1KY<=' W-WDT1F1KY-MAX-X                        
341100                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
341200          DELIMITED BY SIZE INTO SSA1                                     
341300     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
341400     CALL CBLTDLI USING GN WDT1F-PCB DLI-IO-WDT1F1 SSA1                   
341500     MOVE WDT1F-STATUS-CODE TO STATUS-WS                                  
341600     PERFORM IMS-STATUSCHECK                                              
341700     .                                                                    
341800     SKIP3                                                                
341900 IMS-GU-WDT1G1 SECTION.                                                   
342000     STRING 'WDT1G1  (WDT1G1KY>=' W-WDT1G1KY-MIN-X                        
342100                    '&WDT1G1KY<=' W-WDT1G1KY-MAX-X ')'                    
342200          DELIMITED BY SIZE INTO SSA1                                     
342300     MOVE '  GE' TO GOOD-STATUSCODES                                      
342400     CALL CBLTDLI USING GU WDT1G-PCB DLI-IO-WDT1G1 SSA1                   
342500     MOVE WDT1G-STATUS-CODE TO STATUS-WS                                  
342600     PERFORM IMS-STATUSCHECK                                              
342700     .                                                                    
342800     SKIP3                                                                
342900 IMS-GN-WDT1G1 SECTION.                                                   
343000     STRING 'WDT1G1  (WDT1G1KY>=' W-WDT1G1KY-MIN-X                        
343100                    '&WDT1G1KY<=' W-WDT1G1KY-MAX-X ')'                    
343200          DELIMITED BY SIZE INTO SSA1                                     
343300     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
343400     CALL CBLTDLI USING GN WDT1G-PCB DLI-IO-WDT1G1 SSA1                   
343500     MOVE WDT1G-STATUS-CODE TO STATUS-WS                                  
343600     PERFORM IMS-STATUSCHECK                                              
343700     .                                                                    
343800     SKIP3                                                                
343900 IMS-GU-WDT1G1-STATUS SECTION.                                            
344000     STRING 'WDT1G1  (WDT1G1KY>=' W-WDT1G1KY-MIN-X                        
344100                    '&WDT1G1KY<=' W-WDT1G1KY-MAX-X                        
344200                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
344300          DELIMITED BY SIZE INTO SSA1                                     
344400     MOVE '  GE' TO GOOD-STATUSCODES                                      
344500     CALL CBLTDLI USING GU WDT1G-PCB DLI-IO-WDT1G1 SSA1                   
344600     MOVE WDT1G-STATUS-CODE TO STATUS-WS                                  
344700     PERFORM IMS-STATUSCHECK                                              
344800     .                                                                    
344900     SKIP3                                                                
345000 IMS-GN-WDT1G1-STATUS SECTION.                                            
345100     STRING 'WDT1G1  (WDT1G1KY>=' W-WDT1G1KY-MIN-X                        
345200                    '&WDT1G1KY<=' W-WDT1G1KY-MAX-X                        
345300                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
345400          DELIMITED BY SIZE INTO SSA1                                     
345500     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
345600     CALL CBLTDLI USING GN WDT1G-PCB DLI-IO-WDT1G1 SSA1                   
345700     MOVE WDT1G-STATUS-CODE TO STATUS-WS                                  
345800     PERFORM IMS-STATUSCHECK                                              
345900     .                                                                    
346000     SKIP3                                                                
346100 IMS-GU-WDT1H1 SECTION.                                                   
346200     STRING 'WDT1H1  (WDT1H1KY>=' W-WDT1H1KY-MIN-X                        
346300                    '&WDT1H1KY<=' W-WDT1H1KY-MAX-X ')'                    
346400          DELIMITED BY SIZE INTO SSA1                                     
346500     MOVE '  GE' TO GOOD-STATUSCODES                                      
346600     CALL CBLTDLI USING GU WDT1H-PCB DLI-IO-WDT1H1 SSA1                   
346700     MOVE WDT1H-STATUS-CODE TO STATUS-WS                                  
346800     PERFORM IMS-STATUSCHECK                                              
346900     .                                                                    
347000     SKIP3                                                                
347100 IMS-GN-WDT1H1 SECTION.                                                   
347200     STRING 'WDT1H1  (WDT1H1KY>=' W-WDT1H1KY-MIN-X                        
347300                    '&WDT1H1KY<=' W-WDT1H1KY-MAX-X ')'                    
347400          DELIMITED BY SIZE INTO SSA1                                     
347500     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
347600     CALL CBLTDLI USING GN WDT1H-PCB DLI-IO-WDT1H1 SSA1                   
347700     MOVE WDT1H-STATUS-CODE TO STATUS-WS                                  
347800     PERFORM IMS-STATUSCHECK                                              
347900     .                                                                    
348000     SKIP3                                                                
348100 IMS-GU-WDT1H1-STATUS SECTION.                                            
348200     STRING 'WDT1H1  (WDT1H1KY>=' W-WDT1H1KY-MIN-X                        
348300                    '&WDT1H1KY<=' W-WDT1H1KY-MAX-X                        
348400                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
348500          DELIMITED BY SIZE INTO SSA1                                     
348600     MOVE '  GE' TO GOOD-STATUSCODES                                      
348700     CALL CBLTDLI USING GU WDT1H-PCB DLI-IO-WDT1H1 SSA1                   
348800     MOVE WDT1H-STATUS-CODE TO STATUS-WS                                  
348900     PERFORM IMS-STATUSCHECK                                              
349000     .                                                                    
349100     SKIP3                                                                
349200 IMS-GN-WDT1H1-STATUS SECTION.                                            
349300     STRING 'WDT1H1  (WDT1H1KY>=' W-WDT1H1KY-MIN-X                        
349400                    '&WDT1H1KY<=' W-WDT1H1KY-MAX-X                        
349500                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
349600          DELIMITED BY SIZE INTO SSA1                                     
349700     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
349800     CALL CBLTDLI USING GN WDT1H-PCB DLI-IO-WDT1H1 SSA1                   
349900     MOVE WDT1H-STATUS-CODE TO STATUS-WS                                  
350000     PERFORM IMS-STATUSCHECK                                              
350100     .                                                                    
350200     SKIP3                                                                
350300 IMS-GU-WDT1I1 SECTION.                                                   
350400     STRING 'WDT1I1  (WDT1I1KY>=' W-WDT1I1KY-MIN-X                        
350500                    '&WDT1I1KY<=' W-WDT1I1KY-MAX-X ')'                    
350600          DELIMITED BY SIZE INTO SSA1                                     
350700     MOVE '  GE' TO GOOD-STATUSCODES                                      
350800     CALL CBLTDLI USING GU WDT1I-PCB DLI-IO-WDT1I1 SSA1                   
350900     MOVE WDT1I-STATUS-CODE TO STATUS-WS                                  
351000     PERFORM IMS-STATUSCHECK                                              
351100     .                                                                    
351200     SKIP3                                                                
351300 IMS-GN-WDT1I1 SECTION.                                                   
351400     STRING 'WDT1I1  (WDT1I1KY>=' W-WDT1I1KY-MIN-X                        
351500                    '&WDT1I1KY<=' W-WDT1I1KY-MAX-X ')'                    
351600          DELIMITED BY SIZE INTO SSA1                                     
351700     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
351800     CALL CBLTDLI USING GN WDT1I-PCB DLI-IO-WDT1I1 SSA1                   
351900     MOVE WDT1I-STATUS-CODE TO STATUS-WS                                  
352000     PERFORM IMS-STATUSCHECK                                              
352100     .                                                                    
352200     SKIP3                                                                
352300 IMS-GU-WDT1I1-STATUS SECTION.                                            
352400     STRING 'WDT1I1  (WDT1I1KY>=' W-WDT1I1KY-MIN-X                        
352500                    '&WDT1I1KY<=' W-WDT1I1KY-MAX-X                        
352600                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
352700          DELIMITED BY SIZE INTO SSA1                                     
352800     MOVE '  GE' TO GOOD-STATUSCODES                                      
352900     CALL CBLTDLI USING GU WDT1I-PCB DLI-IO-WDT1I1 SSA1                   
353000     MOVE WDT1I-STATUS-CODE TO STATUS-WS                                  
353100     PERFORM IMS-STATUSCHECK                                              
353200     .                                                                    
353300     SKIP3                                                                
353400 IMS-GN-WDT1I1-STATUS SECTION.                                            
353500     STRING 'WDT1I1  (WDT1I1KY>=' W-WDT1I1KY-MIN-X                        
353600                    '&WDT1I1KY<=' W-WDT1I1KY-MAX-X                        
353700                    '&KDSTAPF  =' W-KDSTAPF        ')'                    
353800          DELIMITED BY SIZE INTO SSA1                                     
353900     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
354000     CALL CBLTDLI USING GN WDT1I-PCB DLI-IO-WDT1I1 SSA1                   
354100     MOVE WDT1I-STATUS-CODE TO STATUS-WS                                  
354200     PERFORM IMS-STATUSCHECK                                              
354300     .                                                                    
354400     SKIP3                                                                
354500 IMS-DLET-WDT101 SECTION.                                                 
354600     MOVE '  ' TO GOOD-STATUSCODES                                        
354700     CALL CBLTDLI USING DLET WDT1-PCB DLI-IO-WDT101                       
354800     MOVE WDT1-STATUS-CODE TO STATUS-WS                                   
354900     PERFORM IMS-STATUSCHECK                                              
355000     .                                                                    
355100     EJECT                                                                
355200     SKIP3                                                                
355300 IMS-REPL-WDT101 SECTION.                                                 
355400     MOVE '  ' TO GOOD-STATUSCODES                                        
355500     CALL CBLTDLI USING REPL WDT1-PCB DLI-IO-WDT101                       
355600     MOVE WDT1-STATUS-CODE TO STATUS-WS                                   
355700     PERFORM IMS-STATUSCHECK                                              
355800     .                                                                    
355900     SKIP3                                                                
356000     EJECT                                                                
356100 IMS-GU-WDK611 SECTION.                                                   
356200     MOVE SPACES TO SSA1                                                  
356300     MOVE SPACES TO SSA2                                                  
356400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
356500          DELIMITED BY SIZE INTO SSA1                                     
356600     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
356700          DELIMITED BY SIZE INTO SSA2                                     
356800     MOVE '  GE' TO GOOD-STATUSCODES                                      
356900     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
357000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
357100     PERFORM IMS-STATUSCHECK                                              
357200     .                                                                    
357300     EJECT                                                                
357400 IMS-GHU-WDD811 SECTION.                                                  
357500     STRING 'WDD801  (IDARTNR  =' W-IDARTNR-X ')'                         
357600          DELIMITED BY SIZE INTO SSA1                                     
357700     STRING 'WDD811  (WDD811KY>=' W-WDD811KY-MIN-X                        
357800                    '&WDD811KY<=' W-WDD811KY-MAX-X                        
357900                    '&ADBUFFOM =' W-ADBUFFOM-X                            
358000                    '&ADBUFGAN =' W-ADBUFGAN-X                            
358100                    '&ADBUFPL  =' W-ADBUFPL-X  ')'                        
358200          DELIMITED BY SIZE INTO SSA2                                     
358300     MOVE '  GE' TO GOOD-STATUSCODES                                      
358400     CALL CBLTDLI USING GHU WDD8-PCB DLI-IO-WDD811 SSA1 SSA2              
358500     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
358600     PERFORM IMS-STATUSCHECK                                              
358700     .                                                                    
358800     EJECT                                                                
358900 IMS-REPL-WDD811 SECTION.                                                 
359000     MOVE '  ' TO GOOD-STATUSCODES                                        
359100     CALL CBLTDLI USING REPL WDD8-PCB DLI-IO-WDD811                       
359200     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
359300     PERFORM IMS-STATUSCHECK                                              
359400     .                                                                    
359500     SKIP3                                                                
359600 IMS-DLET-WDD811 SECTION.                                                 
359700     MOVE '  ' TO GOOD-STATUSCODES                                        
359800     CALL CBLTDLI USING DLET WDD8-PCB DLI-IO-WDD811                       
359900     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
360000     PERFORM IMS-STATUSCHECK                                              
360100     .                                                                    
360200     EJECT                                                                
360300 IMS-GU-WDJ901 SECTION.                                                   
360400     STRING 'WDJ901  (IDARTNR  =' W-IDARTNR-X ')'                         
360500          DELIMITED BY SIZE INTO SSA1                                     
360600     MOVE '  GE' TO GOOD-STATUSCODES                                      
360700     CALL CBLTDLI USING GU WDJ9-PCB DLI-IO-WDJ9 SSA1                      
360800     MOVE WDJ9-STATUS-CODE TO STATUS-WS                                   
360900     PERFORM IMS-STATUSCHECK                                              
361000     .                                                                    
361100     EJECT                                                                
361200 IMS-GHNP-WDJ911 SECTION.                                                 
361300     STRING 'WDJ911  (IDDC     =' W-IDDC-X ')'                            
361400             DELIMITED BY SIZE INTO SSA1                                  
361500     MOVE '  GE' TO GOOD-STATUSCODES                                      
361600     CALL CBLTDLI USING GHNP WDJ9-PCB DLI-IO-WDJ9 SSA1                    
361700     MOVE WDJ9-STATUS-CODE TO STATUS-WS                                   
361800     PERFORM IMS-STATUSCHECK                                              
361900     .                                                                    
362000     EJECT                                                                
362100 IMS-REPL-WDJ911 SECTION.                                                 
362200     MOVE '  ' TO GOOD-STATUSCODES                                        
362300     CALL CBLTDLI USING REPL WDJ9-PCB DLI-IO-WDJ9                         
362400     MOVE WDJ9-STATUS-CODE TO STATUS-WS                                   
362500     PERFORM IMS-STATUSCHECK                                              
362600     .                                                                    
362700     EJECT                                                                
362800 IMS-STATUSCHECK SECTION.                                                 
362900     SET STATUS-IX TO 1                                                   
363000     SEARCH GOOD-STATUS                                                   
363100       AT END                                                             
363200         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
363300         DELIMITED BY SIZE INTO ERROR-TEXT                                
363400         CALL FELLOG                                                      
363500       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
363600         CONTINUE                                                         
363700     END-SEARCH                                                           
363800     .                                                                    
