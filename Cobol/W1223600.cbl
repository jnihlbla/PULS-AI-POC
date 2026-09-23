001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     W1223600.                                                
001400 AUTHOR.         ARCHANA BHAT.                                            
001500 DATE-WRITTEN.   14/10/27.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800                                                                          
001900*    FUNCTION:                                                            
002000*        THIS PROGRAM READS W12207 FILE. IF THE RECORD TYPE IN THE        
002100*        FILE IS 'B' OR 'S', THE CORRESPONDING PARTS ARE DELETED          
002200*        FROM WDL6                                                        
002300*                                                                         
002410*        THE PROGRAM UPDATES   WDL6                                       
002500*                                                                         
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003301     SKIP2                                                                
003302*          --- PARTS TO BE DELETED FROM WDL6                              
003310     SELECT W12207                     ASSIGN TO W12236D1.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800 FILE SECTION.                                                            
003901     SKIP3                                                                
003902 FD  W12207                                                               
003903     RECORDING       F                                                    
003904     BLOCK CONTAINS  0.                                                   
003905                                                                          
003910*01  -COPY W12207      -L.                                                
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'W1223600'.            
004300 77  W-DLET-WDL601               PIC 9(7)    VALUE ZERO.                  
004400 01  CHKP-VAR.                                                            
004500     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004600     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004700     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004800     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004900     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
005000     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
005300     SKIP2                                                                
005400 01  ERROR-TEXT.                                                          
005500     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
005600     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
005801                                                                          
005802 77  W12207-EOF-SW               PIC X       VALUE 'N'.                   
005810     88  END-OF-W12207                       VALUE 'Y'.                   
006100     EJECT                                                                
006800 01  GENERAL-SUBPROGRAMS.                                                 
006900*                                                                         
007000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007210     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007301     EJECT                                                                
007302*    --- PARAMETRAR TILL POSTSUM                                          
007303*                                                                         
007310*01  -COPY W0005   -PRE  POSTSUM-                                         
007601     EJECT                                                                
007602 01  IN-AREA-START               PIC X(24)   VALUE                        
007603                                             'IN-AREA-START'.             
007604     SKIP2                                                                
007605                                                                          
007610*01  AREA -COPY W12207     -PRE IN-                                       
007700*                                                                         
007800     EJECT                                                                
007900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008000     SKIP3                                                                
008100 01  KEYS-TILL-DLI.                                                       
008201     03  W-IDARTNR-X.                                                     
008210         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008300     SKIP2                                                                
008400*    --- STATUS-KOD FRÅN IMS                                              
008500 01  STATUS-WS                   PIC XX.                                  
008600     88  SEGMENT-FOUND                       VALUE '  '.                  
008800     88  SEGMENT-MISSING                     VALUE 'GE'.                  
009000     88  IMS-NOT-OK                          VALUE 'XD'.                  
009100     SKIP2                                                                
009200 01  GOOD-STATUSCODES.                                                    
009300     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009400     SKIP3                                                                
009500 01  SSA1                        PIC X(64).                               
009600 01  SSA2                        PIC X(64).                               
009700     EJECT                                                                
009800*    --- IMS FUNCTION CODES                                               
009900*01  -COPY W0003                                                          
010000     EJECT                                                                
010200*    ---  DLI INPUT-OUTPUT AREA                                           
010300                                                                          
010401 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL601'.                      
010402 01  DLI-IO-WDL601.                                                       
010410*    03  -COPY WDL601                                                     
010500                                                                          
010900     EJECT                                                                
011000 LINKAGE SECTION.                                                         
011100                                                                          
011200*01  -COPY W0009   -PRE MSG-                                              
011301                                                                          
011302*01  -COPY W0008  -PRE WDL6-                                              
011310     05  FILLER                  PIC X.                                   
011600     EJECT                                                                
011701 PROCEDURE DIVISION  USING MSG-PCB WDL6-PCB.                              
011702 MAIN SECTION.                                                            
011710     ENTRY 'DLITCBL' USING MSG-PCB WDL6-PCB.                              
011800                                                                          
012000     SKIP2                                                                
012100     PERFORM A-INIT                                                       
012210     PERFORM S01-READ-W12207                                              
012300     PERFORM UNTIL END-OF-W12207                                          
012610       IF IN-UTFIL-TYP = 'S' OR 'B'                                       
012620         MOVE IN-IDARTNR TO W-IDARTNR                                     
012630         PERFORM IMS-GET-WDL601                                           
012640         IF SEGMENT-FOUND                                                 
012650            PERFORM IMS-DLET-WDL601                                       
                  ADD 1 TO W-DLET-WDL601                                        
012660            ADD +1 TO CHKP-ANT                                            
012670         END-IF                                                           
012680         IF CHKP-ANT > CHKP-MAX                                           
012690           PERFORM X-TAKE-CHECKPOINT                                      
012691         END-IF                                                           
012692       END-IF                                                             
012693       PERFORM S01-READ-W12207                                            
013400     END-PERFORM                                                          
013600                                                                          
013700     PERFORM Z-FINIT                                                      
013800                                                                          
013900     MOVE ZERO TO RETURN-CODE                                             
014000     GOBACK                                                               
014100     .                                                                    
014200     EJECT                                                                
014300 A-INIT SECTION.                                                          
014400     SKIP2                                                                
014500                                                                          
014600     PERFORM IMS-RESTART                                                  
014810     OPEN INPUT W12207                                                    
015510     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015800     .                                                                    
016000     EJECT                                                                
016100 Z-FINIT SECTION.                                                         
016200                                                                          
016610     CLOSE W12207                                                         
016802     MOVE 'S' TO POSTSUM-OPKOD                                            
016810     CALL POSTSUM USING POSTSUM-PARM                                      
017000     .                                                                    
017101     EJECT                                                                
017102 S01-READ-W12207  SECTION.                                                
017103     SKIP2                                                                
017104     READ W12207 INTO IN-AREA                                             
017105     AT END                                                               
017107        SET END-OF-W12207 TO TRUE                                         
017108                                                                          
017109     NOT AT END                                                           
017110        MOVE 'W12207'     TO POSTSUM-FDNAMN                               
017111        MOVE 'W12236D1'   TO POSTSUM-DDNAMN2                              
017112        MOVE IN-UTFIL-TYP TO POSTSUM-TRANSTYP                             
017113        CALL POSTSUM USING POSTSUM-PARM                                   
017116     END-READ                                                             
017120     .                                                                    
017400     EJECT                                                                
017500 X-TAKE-CHECKPOINT   SECTION.                                             
017600                                                                          
018200     PERFORM IMS-CHECKPOINT                                               
018300     MOVE ZERO TO CHKP-ANT                                                
018500     .                                                                    
018600     EJECT                                                                
018700* --- IMS SECTIONS  ---                                                   
018800                                                                          
018901     EJECT                                                                
018902 IMS-GET-WDL601 SECTION.                                                  
018903                                                                          
018904     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
018905          DELIMITED BY SIZE INTO SSA1                                     
018906     MOVE '  GE' TO GOOD-STATUSCODES                                      
018907     CALL CBLTDLI USING GHU WDL6-PCB DLI-IO-WDL601 SSA1                   
018908     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
018909     PERFORM IMS-STATUSCHECK                                              
018910     .                                                                    
018911     SKIP3                                                                
018912 IMS-DLET-WDL601 SECTION.                                                 
018913                                                                          
018914     MOVE '  ' TO GOOD-STATUSCODES                                        
018915     CALL CBLTDLI USING DLET WDL6-PCB DLI-IO-WDL601                       
018916     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
018917     PERFORM IMS-STATUSCHECK                                              
018920     .                                                                    
019000     EJECT                                                                
019100 IMS-RESTART SECTION.                                                     
019200     SKIP2                                                                
019300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
019400     MOVE '  ' TO GOOD-STATUSCODES                                        
019500     CALL CBLTDLI USING XRST MSG-PCB                                      
019600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
019700                        CHKP-AREA-LENGTH CHKP-AREA                        
019800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
019900     PERFORM IMS-STATUSCHECK                                              
020000     .                                                                    
020100     SKIP3                                                                
020200 IMS-CHECKPOINT SECTION.                                                  
020300     SKIP2                                                                
020400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
020500     MOVE '  XD' TO GOOD-STATUSCODES                                      
020600     CALL CBLTDLI USING CHKP MSG-PCB                                      
020700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
020800                        CHKP-AREA-LENGTH CHKP-AREA                        
020900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
021000     PERFORM IMS-STATUSCHECK                                              
021100                                                                          
021200     IF IMS-NOT-OK                                                        
021300       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE' TO                     
021310                                                   ERROR-TEXT-STR         
021400       DISPLAY ERROR-TEXT                                                 
021500       CALL FELLOG                                                        
021600     END-IF                                                               
021700     .                                                                    
021800     EJECT                                                                
021900 IMS-STATUSCHECK SECTION.                                                 
022000     SKIP2                                                                
022100     SET STATUS-IX TO 1                                                   
022200     SEARCH GOOD-STATUS                                                   
022300       AT END                                                             
022400         STRING ' INVALID STATUS CODE FROM IMS:' STATUS-WS                
022500           DELIMITED BY SIZE INTO ERROR-TEXT                              
022600         DISPLAY ERROR-TEXT                                               
022700         CALL FELLOG                                                      
022800       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
022900         CONTINUE                                                         
023000     END-SEARCH                                                           
023100     .                                                                    
