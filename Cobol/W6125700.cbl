000100*********************************************                             
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W6125700.                                                
000400 AUTHOR.         SRINADH NADIMPALLI.                                      
000500 DATE-WRITTEN.   22/08/08.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNCTION:                                                            
000900*        WDK2 TO AZURE                                                    
001000*                                                                         
001100*        THE PROGRAM READS     WDK2                                       
001200*                                                                         
001300*    ABENDCODES:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- WDK2 EXTRACT TO AZURE                                      
002600     SELECT W61257                     ASSIGN TO W61257D1.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP2                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  W61257                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500                                                                          
003600*01  RECORD -COPY W61257X -PRE  UT-  -L.                                  
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
004000 77  IDPGM                       PIC X(8)    VALUE 'W6125700'.            
004100 77  YES                         PIC X       VALUE 'J'.                   
004200 77  NOO                         PIC X       VALUE 'N'.                   
004300     EJECT                                                                
004400 01  211-PRESENT                 PIC X.                                   
004500     88  211-NOK                             VALUE 'N'.                   
004600 01  212-PRESENT                 PIC X.                                   
004700     88  212-NOK                             VALUE 'N'.                   
004800 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
004900 01  FILLER REDEFINES TODAYS-DATE.                                        
005000     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005100     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005200     03  TODAYS-DATE-DAY         PIC 9(2).                                
005300     EJECT                                                                
005400 01  GENERAL-SUBPROGRAMS.                                                 
005500*                                                                         
005600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006000     SKIP2                                                                
006100*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006200                                                                          
006300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006600     SKIP2                                                                
006700 01  ERROR-TEXT.                                                          
006800     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
006900     03  ERROR-TEXT-STR          PIC X(70)   VALUE SPACE.                 
007000     EJECT                                                                
007100*    --- PARAMETRAR TILL POSTSUM                                          
007200*                                                                         
007300*01  -COPY W0005   -PRE  POSTSUM-                                         
007310 01  UT-TRANSID.                                                          
007320     03  FILLER                  PIC X(6)  VALUE 'W61257'.                
007330     03  FILLER                  PIC X(8)  VALUE 'W61257D1'.              
007340     03  FILLER                  PIC X(4)  VALUE 'POST'.                  
007350     EJECT                                                                
007500 01  UT-AREA-START               PIC X(24)   VALUE                        
007600                                 'UT-AREA-START  '.                       
007700     SKIP2                                                                
007800                                                                          
007900 01  AREA -COPY W61257X    -PRE UT-                                       
008500     EJECT                                                                
008600*    --- AREAS FOR IMS-SECTIONS                                           
008700                                                                          
008800     EJECT                                                                
008900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009000     SKIP3                                                                
009100 01  KEYS-FOR-DLI.                                                        
009200     03  W-IDARTNR-X.                                                     
009300         05  W-IDARTNR           PIC S9(5)   VALUE ZERO COMP-3.           
009400     03  W-KDSEGKEY-X.                                                    
009500         05  W-KDSEGKEY          PIC X(1)    VALUE SPACE.                 
009600     03  W-KDSEGKEY-X.                                                    
009700         05  W-KDSEGKEY          PIC X(1)    VALUE SPACE.                 
009800     SKIP2                                                                
009900*    --- STATUS-KOD FRÅN IMS                                              
010000 01  STATUS-WS                   PIC XX.                                  
010100     88  SEGMENT-FOUND                       VALUE '  '.                  
010200     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
010300     88  SEGMENT-MISSING                     VALUE 'GB'.                  
010400     88  SEGMENT-NEXT-PARENT                 VALUE 'GA'.                  
010500     SKIP2                                                                
010600 01  GOOD-STATUSCODES.                                                    
010700     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010800     SKIP3                                                                
010900 01  SSA1                        PIC X(64).                               
011000 01  SSA2                        PIC X(64).                               
011100     EJECT                                                                
011200*    --- IMS FUNCTION CODES                                               
011300*01  -COPY W0003                                                          
011400     EJECT                                                                
011500*    ---  DLI INPUT-OUTPUT AREA                                           
011600 01  DLI-IO-AREA.                                                         
011700     03  IO-AREA PIC X(150).                                              
011800     SKIP3                                                                
011900*    03 FILLER  -COPY WDK201 -RED IO-AREA                                 
012000     EJECT                                                                
012100*    03 FILLER  -COPY WDK211 -RED IO-AREA                                 
012200     EJECT                                                                
012300*    03 FILLER  -COPY WDK212 -RED IO-AREA                                 
012400     EJECT                                                                
012500 LINKAGE SECTION.                                                         
012600                                                                          
012700                                                                          
012800*01  -COPY W0008  -PRE WDK2-                                              
012900     05  FILLER                  PIC X.                                   
013000     EJECT                                                                
013100 PROCEDURE DIVISION  USING WDK2-PCB.                                      
013200 MAIN SECTION.                                                            
013300     ENTRY 'DLITCBL' USING WDK2-PCB.                                      
013500                                                                          
013600     PERFORM A-INIT                                                       
013800     PERFORM IMS-GET-WDK2                                                 
013900     PERFORM UNTIL SEGMENT-MISSING                                        
014000      EVALUATE WDK2-SEG-NAME-FB                                           
014100       WHEN 'WDK201  '                                                    
014200         MOVE ARTM-IDARTNR TO UT-ARTM-IDARTNR                             
014300       WHEN 'WDK211  '                                                    
014400         PERFORM B-MOVE-WDK211                                            
015700       WHEN 'WDK212  '                                                    
015710         PERFORM C-MOVE-WDK212                                            
016300       WHEN OTHER                                                         
016400        CONTINUE                                                          
016500      END-EVALUATE                                                        
016600      PERFORM IMS-GET-WDK2                                                
016700      IF SEGMENT-NEXT-PARENT OR SEGMENT-MISSING                           
016800       PERFORM D-CHECK-WRITE                                              
016900      END-IF                                                              
017000     END-PERFORM                                                          
017100                                                                          
017200                                                                          
017300     PERFORM Z-FINIT                                                      
017400                                                                          
017500     MOVE ZERO TO RETURN-CODE                                             
017600     GOBACK                                                               
017700     .                                                                    
017800     EJECT                                                                
017900 A-INIT SECTION.                                                          
018000                                                                          
018100     OPEN OUTPUT W61257                                                   
018200                                                                          
018300     ACCEPT TODAYS-DATE  FROM DATE                                        
018400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018500     MOVE NOO TO 211-PRESENT                                              
018510     MOVE NOO TO 212-PRESENT                                              
018600     .                                                                    
018700     EJECT                                                                
018710 B-MOVE-WDK211 SECTION.                                                   
018712     MOVE MATD-KDVSOP       TO UT-MATD-KDVSOP                             
018713     MOVE MATD-KVHEIGHT-BTO TO UT-MATD-KVHEIGHT-BTO                       
018714     MOVE MATD-KVHEIGHT-NTO TO UT-MATD-KVHEIGHT-NTO                       
018715     MOVE MATD-KVLENGTH-BTO TO UT-MATD-KVLENGTH-BTO                       
018716     MOVE MATD-KVLENGTH-NTO TO UT-MATD-KVLENGTH-NTO                       
018717     MOVE MATD-KVWIDTH-BTO  TO UT-MATD-KVWIDTH-BTO                        
018718     MOVE MATD-KVWIDTH-NTO  TO UT-MATD-KVWIDTH-NTO                        
018719     MOVE MATD-TIUPPDAT     TO UT-MATD-TIUPPDAT                           
018720     MOVE MATD-VKART-BTO    TO UT-MATD-VKART-BTO                          
018721     MOVE MATD-VKART-NTO    TO UT-MATD-VKART-NTO                          
018722     MOVE MATD-VLARTNTO     TO UT-MATD-VLARTNTO                           
018723     MOVE YES               TO 211-PRESENT                                
018724     .                                                                    
018730     EJECT                                                                
018740 C-MOVE-WDK212 SECTION.                                                   
018742     MOVE KDP-KVANTAL TO UT-KDP-KVANTAL                                   
018743     MOVE KDP-TIUPPDAT TO UT-KDP-TIUPPDAT                                 
018744     MOVE KDP-VKART-NTO TO UT-KDP-VKART-NTO                               
018745     MOVE YES           TO 212-PRESENT                                    
018750     .                                                                    
018760     EJECT                                                                
018800 Z-FINIT SECTION.                                                         
018900     CLOSE W61257                                                         
019000     SKIP2                                                                
019100     MOVE 'S' TO POSTSUM-OPKOD                                            
019200     CALL POSTSUM USING POSTSUM-PARM                                      
019300     .                                                                    
019400     EJECT                                                                
019500 D-CHECK-WRITE SECTION.                                                   
019600     IF 211-NOK                                                           
019800         MOVE ZERO              TO UT-MATD-KDVSOP                         
019900         MOVE ZERO              TO UT-MATD-KVHEIGHT-BTO                   
020000         MOVE ZERO              TO UT-MATD-KVHEIGHT-NTO                   
020100         MOVE ZERO              TO UT-MATD-KVLENGTH-BTO                   
020200         MOVE ZERO              TO UT-MATD-KVLENGTH-NTO                   
020300         MOVE ZERO              TO UT-MATD-KVWIDTH-BTO                    
020400         MOVE ZERO              TO UT-MATD-KVWIDTH-NTO                    
020500         MOVE ZERO              TO UT-MATD-TIUPPDAT                       
020600         MOVE ZERO              TO UT-MATD-VKART-BTO                      
020700         MOVE ZERO              TO UT-MATD-VKART-NTO                      
020800         MOVE ZERO              TO UT-MATD-VLARTNTO                       
020900     END-IF                                                               
021000     IF 212-NOK                                                           
021200         MOVE ZERO        TO UT-KDP-KVANTAL                               
021300         MOVE ZERO         TO UT-KDP-TIUPPDAT                             
021400         MOVE ZERO          TO UT-KDP-VKART-NTO                           
021500     END-IF                                                               
021600     PERFORM S11-WRITE-W61257                                             
021700     MOVE NOO TO 211-PRESENT                                              
021800     MOVE NOO TO 212-PRESENT                                              
021900     .                                                                    
022000     EJECT                                                                
022100 S11-WRITE-W61257 SECTION.                                                
022200                                                                          
022300     WRITE UT-RECORD FROM UT-AREA                                         
022400                                                                          
022410     MOVE UT-TRANSID      TO POSTSUM-TRANSID                              
022800     CALL POSTSUM USING POSTSUM-PARM                                      
022900     .                                                                    
023000     EJECT                                                                
023100 S99-ABEND SECTION.                                                       
023200                                                                          
023300     SKIP2                                                                
023400     MOVE 'S' TO POSTSUM-OPKOD                                            
023500     CALL POSTSUM USING POSTSUM-PARM                                      
023600     CALL ABEND USING RKOD-ABEND                                          
023700     .                                                                    
023800     EJECT                                                                
023900* --- IMS SECTIONS  ---                                                   
024000                                                                          
024100     EJECT                                                                
024200 IMS-GET-WDK2 SECTION.                                                    
024300                                                                          
024400     MOVE '  GAGKGB' TO GOOD-STATUSCODES                                  
024500     CALL CBLTDLI USING GN WDK2-PCB DLI-IO-AREA                           
024600     MOVE WDK2-STATUS-CODE TO STATUS-WS                                   
024700     PERFORM IMS-STATUSCHECK                                              
024800     .                                                                    
024900     EJECT                                                                
025000 IMS-STATUSCHECK SECTION.                                                 
025100                                                                          
025200     SET STATUS-IX TO 1                                                   
025300     SEARCH GOOD-STATUS                                                   
025400       AT END                                                             
025500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
025600           DELIMITED BY SIZE INTO ERROR-TEXT                              
025700         DISPLAY ERROR-TEXT                                               
025800         CALL FELLOG                                                      
025900       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
026000         CONTINUE                                                         
026100     END-SEARCH                                                           
026200     .                                                                    
