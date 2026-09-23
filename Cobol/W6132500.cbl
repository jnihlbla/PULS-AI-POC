001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     W6132500.                                                
001400 AUTHOR.         UMESH JAIN.                                              
001500 DATE-WRITTEN.   08/11/14.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800                                                                          
001900*    FUNCTION:                                                            
002000*        DELETE ALL THE APPROVED SEGMENTS FROM WDT1 DATABSE.              
002100*        THE INPUT FILE IS W613.W613V3.W61322(+0) AND FROM PROGRAM        
002200*        W6132200.                                                        
002300*                                                                         
002410*        THE PROGRAM UPDATES WDT1                                         
002500*                                                                         
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003301     SKIP2                                                                
003302*          --- ALL APPROVED SEGMENTS FROM WDT1 DATABASE                   
003310     SELECT W61322                     ASSIGN TO W61325D1.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800 FILE SECTION.                                                            
003901     SKIP3                                                                
003902 FD  W61322                                                               
003903     RECORDING       F                                                    
003904     BLOCK CONTAINS  0.                                                   
003905                                                                          
003910*01  -COPY WDT101      -L.                                                
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'W6132500'.            
004400 01  CHKP-VAR.                                                            
004500     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004600     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004700     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004800     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004900     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
005000     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
005100 77  YES                         PIC X       VALUE 'J'.                   
005200 77  NOO                         PIC X       VALUE 'N'.                   
005300     SKIP2                                                                
005400 01  ERRTEXT.                                                             
005500     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
005600     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
005801                                                                          
005802 77  W61322-EOF-SW               PIC X       VALUE 'N'.                   
005810     88  END-OF-W61322                       VALUE 'Y'.                   
006100     EJECT                                                                
006200 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006300 01  FILLER REDEFINES TODAYS-DATE.                                        
006400     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006500     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006600     03  TODAYS-DATE-DAY         PIC 9(2).                                
006700     EJECT                                                                
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
007602 01  W61322-AREA-START           PIC X(24)   VALUE                        
007603                                             'W61322-AREA-START'.         
007604     SKIP2                                                                
007605                                                                          
007610*01  AREA -COPY WDT101     -PRE W61322-                                   
007700*                                                                         
007800     EJECT                                                                
007900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008000     SKIP3                                                                
008100 01  KEYS-TILL-DLI.                                                       
008201     03  W-WDT101KY-X.                                                    
008220         05 W-PF-IDDC            PIC X(2)    VALUE SPACE.                 
008230         05 W-PF-IDARTNR         PIC S9(9)   COMP-3 VALUE ZERO.           
008240         05 W-PF-TIORDTIME       PIC 9(12)   VALUE ZERO.                  
008300     SKIP2                                                                
008400*    --- STATUS-KOD FRÅN IMS                                              
008500 01  STATUS-WS                   PIC XX.                                  
008600     88  SEGMENT-FOUND                       VALUE '  '.                  
008700     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
008800     88  SEGMENT-MISSING                     VALUE 'GE'.                  
008900     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
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
010401 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDT101'.                      
010402 01  DLI-IO-WDT101.                                                       
010410*    03  -COPY WDT101                                                     
010500                                                                          
010900     EJECT                                                                
011000 LINKAGE SECTION.                                                         
011100                                                                          
011200*01  -COPY W0009   -PRE MSG-                                              
011301                                                                          
011302*01  -COPY W0008  -PRE WDT1-                                              
011310     05  FILLER                  PIC X.                                   
011600     EJECT                                                                
011701 PROCEDURE DIVISION  USING MSG-PCB WDT1-PCB.                              
011702 MAIN SECTION.                                                            
011710     ENTRY 'DLITCBL' USING MSG-PCB WDT1-PCB.                              
011800                                                                          
012000     SKIP2                                                                
012100     PERFORM A-INIT                                                       
012210     PERFORM S01-READ-W61322                                              
012300     PERFORM UNTIL END-OF-W61322                                          
012400       IF CHKP-ANT > CHKP-MAX                                             
012500         PERFORM X-TAKE-CHECKPOINT                                        
012600       END-IF                                                             
012700                                                                          
012710       MOVE W61322-PF-IDDC      TO W-PF-IDDC                              
012711       MOVE W61322-PF-IDARTNR   TO W-PF-IDARTNR                           
012720       MOVE W61322-PF-TIORDTIME TO W-PF-TIORDTIME                         
012721                                                                          
012730       PERFORM IMS-GHU-WDT101                                             
012740       IF SEGMENT-FOUND                                                   
012750          PERFORM IMS-DLET-WDT101                                         
012751          ADD +3 TO CHKP-ANT                                              
012760       END-IF                                                             
012800                                                                          
013310       PERFORM S01-READ-W61322                                            
013400     END-PERFORM                                                          
013500                                                                          
013700     PERFORM Z-FINIT                                                      
013800                                                                          
013900     MOVE ZERO TO RETURN-CODE                                             
014000     GOBACK                                                               
014100     .                                                                    
014200     EJECT                                                                
014300 A-INIT SECTION.                                                          
014400     SKIP2                                                                
014600     PERFORM IMS-RESTART                                                  
014810     OPEN INPUT W61322                                                    
015510     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015800     .                                                                    
016000     EJECT                                                                
016100 Z-FINIT SECTION.                                                         
016610     CLOSE W61322                                                         
016801     SKIP2                                                                
016802     MOVE 'S' TO POSTSUM-OPKOD                                            
016810     CALL POSTSUM USING POSTSUM-PARM                                      
017000     .                                                                    
017101     EJECT                                                                
017102 S01-READ-W61322  SECTION.                                                
017103     SKIP2                                                                
017104     READ W61322 INTO W61322-AREA                                         
017105     AT END                                                               
017107        SET END-OF-W61322 TO TRUE                                         
017109     NOT AT END                                                           
017110        MOVE 'W61322' TO POSTSUM-FDNAMN                                   
017111        MOVE 'W61325D1' TO POSTSUM-DDNAMN2                                
017112        MOVE SPACE         TO POSTSUM-TRANSTYP                            
017113        CALL POSTSUM USING POSTSUM-PARM                                   
017116     END-READ                                                             
017120     .                                                                    
017400     EJECT                                                                
017500 X-TAKE-CHECKPOINT   SECTION.                                             
017600                                                                          
017700* --- AT CHECKPOINT YOU LOSE GN-POSITION IN THE BASE                      
017800* --- SAVE DATABASE KEYS IF NECESSARY                                     
018200     PERFORM IMS-CHECKPOINT                                               
018300     MOVE ZERO TO CHKP-ANT                                                
018400* --- REREAD DATABASE IF NECESSARY                                        
018500     .                                                                    
018600     EJECT                                                                
018700* --- IMS SECTIONS  ---                                                   
018800                                                                          
018901     EJECT                                                                
018902 IMS-GHU-WDT101 SECTION.                                                  
018903                                                                          
018904     STRING 'WDT101  (WDT101KY =' W-WDT101KY-X ')'                        
018905          DELIMITED BY SIZE INTO SSA1                                     
018906     MOVE '  GE' TO GOOD-STATUSCODES                                      
018907     CALL CBLTDLI USING GHU WDT1-PCB DLI-IO-WDT101 SSA1                   
018908     MOVE WDT1-STATUS-CODE TO STATUS-WS                                   
018909     PERFORM IMS-STATUSCHECK                                              
018910     .                                                                    
018911     SKIP3                                                                
018912 IMS-DLET-WDT101 SECTION.                                                 
018913                                                                          
018914     MOVE '  ' TO GOOD-STATUSCODES                                        
018915     CALL CBLTDLI USING DLET WDT1-PCB DLI-IO-WDT101                       
018916     MOVE WDT1-STATUS-CODE TO STATUS-WS                                   
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
021300       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE' TO ERRTEXT-STR         
021400       DISPLAY ERRTEXT                                                    
021500       CALL FELLOG                                                        
021600     END-IF                                                               
021700     .                                                                    
021800     EJECT                                                                
021900 IMS-STATUSCHECK SECTION.                                                 
022000     SKIP2                                                                
022100     SET STATUS-IX TO 1                                                   
022200     SEARCH GOOD-STATUS                                                   
022300       AT END                                                             
022400         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
022500           DELIMITED BY SIZE INTO ERRTEXT                                 
022600         DISPLAY ERRTEXT                                                  
022700         CALL FELLOG                                                      
022800       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
022900         CONTINUE                                                         
023000     END-SEARCH                                                           
023100     .                                                                    
