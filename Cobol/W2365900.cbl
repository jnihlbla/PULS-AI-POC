001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     W2365900.                                                
001400 AUTHOR.         CHESTER COUCH.                                           
001500 DATE-WRITTEN.   20/11/04.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800                                                                          
001900*    FUNKTION:                                                            
002000*        LÄSER EN INPUTFIL MED KDOTFREK-INFO OCH                          
002100*        UPPDATERAR SEDAN KDOTFREK FÖR BERÖRD ARTIKEL                     
002200*        PÅ WDK611-SEGMENTET.                                             
002300*                                                                         
002410*        PROGRAMMET UPPDATERAR WDK6                                       
002500*                                                                         
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003301     SKIP2                                                                
003302*          --- KDOTFREK PER ARTIKEL                                       
003303     SELECT W23658                     ASSIGN TO W23659D1.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800 FILE SECTION.                                                            
003901     SKIP3                                                                
003902 FD  W23658                                                               
003903     RECORDING       F                                                    
003904     BLOCK CONTAINS  0.                                                   
003905                                                                          
003906*01  -COPY W2365801      -L.                                              
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'W2365900'.            
004400 01  CHKP-VAR.                                                            
004500     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004600     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004700     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004800     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004900     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
005000**** 03 CHKP-MAX                 PIC S9(3)   VALUE +900 COMP-3.           
005010     03 CHKP-MAX                 PIC S9(3)   VALUE +50  COMP-3.           
005100 77  JA                          PIC X       VALUE 'J'.                   
005200 77  NEJ                         PIC X       VALUE 'N'.                   
005300     SKIP2                                                                
005400 01  FELTEXT.                                                             
005500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005801                                                                          
005802 77  W23658-EOF-SW               PIC X       VALUE 'N'.                   
005810     88  END-OF-W23658                       VALUE 'J'.                   
006100     EJECT                                                                
006200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006300 01  FILLER REDEFINES DAGENS-DATUM.                                       
006400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006700     EJECT                                                                
006800 01  DYNAMISKA-SUBPROGRAM.                                                
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
007606*01  AREA -COPY W2365801     -PRE IN-                                     
007607     EJECT                                                                
007900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008000     SKIP3                                                                
008100 01  NYCKLAR-TILL-DLI.                                                    
008201     03  W-IDARTNR-X.                                                     
008202         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008203     03  W-KDSEGKEY-X.                                                    
008210         05  W-KDSEGKEY          PIC X(1)    VALUE SPACE.                 
008300     SKIP2                                                                
008400*    --- STATUS-KOD FRÅN IMS                                              
008500 01  STATUS-WS                   PIC XX.                                  
008600     88  SEGMENT-FINNS                       VALUE '  '.                  
008700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009000     88  IMS-EJ-OK                           VALUE 'XD'.                  
009100     SKIP2                                                                
009200 01  GODK-STATUSKODER.                                                    
009300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009400     SKIP3                                                                
009500 01  SSA1                        PIC X(64).                               
009600 01  SSA2                        PIC X(64).                               
009700     EJECT                                                                
009800*    --- IMS FUNKTIONSKODER                                               
009900*01  -COPY W0003                                                          
010000     EJECT                                                                
010200*    ---  DLI INPUT-OUTPUT AREA                                           
010300                                                                          
010401 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
010402 01  DLI-IO-WDK601.                                                       
010403*    03  -COPY WDK601                                                     
010404     EJECT                                                                
010405 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
010406 01  DLI-IO-WDK611.                                                       
010410*    03  -COPY WDK611                                                     
010500                                                                          
010900     EJECT                                                                
011000 LINKAGE SECTION.                                                         
011100                                                                          
011200*01  -COPY W0009   -PRE MSG-                                              
011301                                                                          
011302*01  -COPY W0008  -PRE WDK6-                                              
011310     05  FILLER                  PIC X.                                   
011600     EJECT                                                                
011701 PROCEDURE DIVISION  USING MSG-PCB WDK6-PCB.                              
011702 MAIN SECTION.                                                            
011710     ENTRY 'DLITCBL' USING MSG-PCB WDK6-PCB.                              
011800                                                                          
012000     SKIP2                                                                
012100     PERFORM A-INIT                                                       
012210     PERFORM S01-LAES-W23658                                              
012300     PERFORM UNTIL END-OF-W23658                                          
012400       IF CHKP-ANT > CHKP-MAX                                             
012500         PERFORM X-TAG-CHECKPOINT                                         
012600       END-IF                                                             
012700                                                                          
012710       MOVE IN-IDARTNR TO W-IDARTNR                                       
012711       PERFORM IMS-GHU-WDK611                                             
012798       IF IN-KDOTFREK    NOT = CLAG-KDOTFREK                              
012800          MOVE IN-KDOTFREK  TO CLAG-KDOTFREK                              
012801          PERFORM IMS-REPL-WDK611                                         
012802          ADD +1            TO CHKP-ANT                                   
012803                                                                          
012804          PERFORM S02-COUNT-REPL-VIA-POSTSUM                              
012805       END-IF                                                             
013200                                                                          
013310       PERFORM S01-LAES-W23658                                            
013400     END-PERFORM                                                          
013500                                                                          
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
014801                                                                          
014810     OPEN INPUT W23658                                                    
015400                                                                          
015510     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015800     .                                                                    
016000     EJECT                                                                
016100 Z-FINIT SECTION.                                                         
016200                                                                          
016601                                                                          
016602     CLOSE W23658                                                         
016603                                                                          
016801     SKIP2                                                                
016802     MOVE 'S' TO POSTSUM-OPKOD                                            
016810     CALL POSTSUM USING POSTSUM-PARM                                      
017000     .                                                                    
017101     EJECT                                                                
017102 S01-LAES-W23658  SECTION.                                                
017103     SKIP2                                                                
017104     READ W23658 INTO IN-AREA                                             
017105     AT END                                                               
017107        SET END-OF-W23658 TO TRUE                                         
017108                                                                          
017109     NOT AT END                                                           
017110        MOVE 'W23658'   TO POSTSUM-FDNAMN                                 
017111        MOVE 'W23659D1' TO POSTSUM-DDNAMN2                                
017112        MOVE 'ALL'      TO POSTSUM-TRANSTYP                               
017113        CALL POSTSUM USING POSTSUM-PARM                                   
017114                                                                          
017116     END-READ                                                             
017120     .                                                                    
017201     EJECT                                                                
017202 S02-COUNT-REPL-VIA-POSTSUM SECTION.                                      
017203     SKIP2                                                                
017209     MOVE 'WDK611'   TO POSTSUM-FDNAMN                                    
017210     MOVE 'KDOTFREK' TO POSTSUM-DDNAMN2                                   
017220     MOVE 'REPL'     TO POSTSUM-TRANSTYP                                  
017230     CALL POSTSUM USING POSTSUM-PARM                                      
017270     .                                                                    
017280     EJECT                                                                
017500 X-TAG-CHECKPOINT   SECTION.                                              
017600                                                                          
017700* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
017800* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
018200     PERFORM IMS-CHECKPOINT                                               
018300     MOVE ZERO TO CHKP-ANT                                                
018400* --- LÄS OM DATABAS OM DET BEHÖVS                                        
018500     .                                                                    
018600     EJECT                                                                
018700* --- IMS SEKTIONER ---                                                   
018800                                                                          
018901     EJECT                                                                
018912 IMS-GHU-WDK611 SECTION.                                                  
018913                                                                          
018914     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
018915          DELIMITED BY SIZE INTO SSA1                                     
018916     MOVE 'WDK611  '         TO SSA2                                      
018917     MOVE '  ' TO GODK-STATUSKODER                                        
018918     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
018919     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
018920     PERFORM IMS-STATUSKONTROLL                                           
018921     .                                                                    
018922     EJECT                                                                
018933 IMS-REPL-WDK611 SECTION.                                                 
018934                                                                          
018935     MOVE '  ' TO GODK-STATUSKODER                                        
018936     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
018937     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
018938     PERFORM IMS-STATUSKONTROLL                                           
018940     .                                                                    
019000     EJECT                                                                
019100 IMS-RESTART SECTION.                                                     
019200     SKIP2                                                                
019300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
019400     MOVE '  ' TO GODK-STATUSKODER                                        
019500     CALL CBLTDLI USING XRST MSG-PCB                                      
019600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
019700                        CHKP-AREA-LENGTH CHKP-AREA                        
019800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
019900     PERFORM IMS-STATUSKONTROLL                                           
020000     .                                                                    
020100     SKIP3                                                                
020200 IMS-CHECKPOINT SECTION.                                                  
020300     SKIP2                                                                
020400     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
020500     MOVE '  XD' TO GODK-STATUSKODER                                      
020600     CALL CBLTDLI USING CHKP MSG-PCB                                      
020700                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
020800                        CHKP-AREA-LENGTH CHKP-AREA                        
020900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
021000     PERFORM IMS-STATUSKONTROLL                                           
021100                                                                          
021200     IF IMS-EJ-OK                                                         
021300       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
021400       DISPLAY FELTEXT                                                    
021500       CALL FELLOG                                                        
021600     END-IF                                                               
021700     .                                                                    
021800     EJECT                                                                
021900 IMS-STATUSKONTROLL SECTION.                                              
022000     SKIP2                                                                
022100     SET STATUS-IX TO 1                                                   
022200     SEARCH GODK-STATUS                                                   
022300       AT END                                                             
022400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
022500           DELIMITED BY SIZE INTO FELTEXT                                 
022600         DISPLAY FELTEXT                                                  
022700         CALL FELLOG                                                      
022800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
022900         CONTINUE                                                         
023000     END-SEARCH                                                           
023100     .                                                                    
