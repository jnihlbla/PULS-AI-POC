000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4639100.                                                
000300 AUTHOR.         BROMANDER LENA.                                          
000400 DATE-WRITTEN.   15/10/15.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER FIL SKAPAD I W46395 MED POSTER DÄR ORDERRADER              
001000*        PÅ WDF4 EJ FINNS PÅ WDE4. DESSA POSTER DELETAS FRÅN              
001100*        WDF411.                                                          
001200*                                                                         
001300*        PROGRAMMET UPPDATERAR WDF4                                       
001400*                                                                         
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*          --- FIL NYCKLAR TILL WDF411-POSTER SOM SKA TAS BORT            
002400     SELECT W4639R                     ASSIGN TO W46391D1.                
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP3                                                                
002800 FILE SECTION.                                                            
002900     SKIP3                                                                
003000 FD  W4639R                                                               
003100     RECORDING       F                                                    
003200     BLOCK CONTAINS  0.                                                   
003300                                                                          
003400*01  -COPY W4639R      -L.                                                
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700                                                                          
003800 77  IDPGM                       PIC X(8)    VALUE 'W4639100'.            
003900 01  CHKP-VAR.                                                            
004000     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004100     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004200     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004300     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004400     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004500     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800     SKIP2                                                                
004900 01  FELTEXT.                                                             
005000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005200                                                                          
005300 77  W4639R-EOF-SW               PIC X       VALUE 'N'.                   
005400     88  END-OF-W4639R                       VALUE 'J'.                   
005500     EJECT                                                                
005600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005700 01  FILLER REDEFINES DAGENS-DATUM.                                       
005800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006100     EJECT                                                                
006200 01  DYNAMISKA-SUBPROGRAM.                                                
006300*                                                                         
006400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006700     EJECT                                                                
006800*    --- PARAMETRAR TILL POSTSUM                                          
006900*                                                                         
007000*01  -COPY W0005   -PRE  POSTSUM-                                         
007100 01  W-W4639R-KVPOST-IN          PIC 9(7)    VALUE ZERO.                  
007200     EJECT                                                                
007300 01  IN-AREA-START               PIC X(24)   VALUE                        
007400                                             'IN-AREA-START'.             
007500     SKIP2                                                                
007600                                                                          
007700*01  AREA -COPY W4639R     -PRE IN-                                       
007800*                                                                         
007900     EJECT                                                                
008000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008100     SKIP3                                                                
008200 01  NYCKLAR-TILL-DLI.                                                    
008300                                                                          
008400   03  W-IDLEVNR-X.                                                       
008500       05 W-IDLEVNR              PIC X(5)     VALUE SPACE.                
008600                                                                          
008700     03  W-WDF411KY-X.                                                    
008800         05 W-IDPRODNR        PIC S9(7) COMP-3  VALUE ZERO.               
008900         05 W-IDPURAD         PIC S9(5) COMP-3  VALUE ZERO.               
009000         05 W-TIUTSKR         PIC S9(7) COMP-3  VALUE ZERO.               
009100     SKIP2                                                                
009200*    --- STATUS-KOD FRÅN IMS                                              
009300 01  STATUS-WS                   PIC XX.                                  
009400     88  SEGMENT-FINNS                       VALUE '  '.                  
009500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009800     88  IMS-EJ-OK                           VALUE 'XD'.                  
009900     SKIP2                                                                
010000 01  GODK-STATUSKODER.                                                    
010100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010200     SKIP3                                                                
010300 01  SSA1                        PIC X(64).                               
010400 01  SSA2                        PIC X(64).                               
010500     EJECT                                                                
010600*    --- IMS FUNKTIONSKODER                                               
010700*01  -COPY W0003                                                          
010800     EJECT                                                                
010900*    ---  DLI INPUT-OUTPUT AREA                                           
011000                                                                          
011100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF411'.                      
011200 01  DLI-IO-WDF411.                                                       
011300*    03  -COPY WDF411                                                     
011400                                                                          
011500     EJECT                                                                
011600 LINKAGE SECTION.                                                         
011700                                                                          
011800*01  -COPY W0009   -PRE MSG-                                              
011900                                                                          
012000*01  -COPY W0008  -PRE WDF4-                                              
012100     05  FILLER                  PIC X.                                   
012200     EJECT                                                                
012300 PROCEDURE DIVISION  USING MSG-PCB WDF4-PCB.                              
012400 MAIN SECTION.                                                            
012500     ENTRY 'DLITCBL' USING MSG-PCB WDF4-PCB.                              
012600                                                                          
012700     SKIP2                                                                
012800     PERFORM A-INIT                                                       
012810     PERFORM IMS-RESTART                                                  
012900     PERFORM S01-LAES-W4639R                                              
013000     PERFORM UNTIL END-OF-W4639R                                          
013100                                                                          
013200       IF CHKP-ANT > CHKP-MAX                                             
013300         PERFORM X-TAG-CHECKPOINT                                         
013400       END-IF                                                             
013500                                                                          
013600       PERFORM B-RENSA-WDF411                                             
013700       PERFORM S01-LAES-W4639R                                            
013800     END-PERFORM                                                          
013900                                                                          
014000                                                                          
014100     PERFORM Z-FINIT                                                      
014200                                                                          
014300     MOVE ZERO TO RETURN-CODE                                             
014400     GOBACK                                                               
014500                                                                          
014600     .                                                                    
014700     EJECT                                                                
014800 A-INIT SECTION.                                                          
014900     SKIP2                                                                
015000                                                                          
015100     OPEN INPUT W4639R                                                    
015200                                                                          
015300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015400     .                                                                    
015500     EJECT                                                                
015600                                                                          
015700 B-RENSA-WDF411 SECTION.                                                  
015800                                                                          
015900     MOVE IN-IDLEVNR     TO W-IDLEVNR                                     
016000     MOVE IN-IDPRODNR    TO W-IDPRODNR                                    
016100     MOVE IN-IDPURAD     TO W-IDPURAD                                     
016200     MOVE IN-TIUTSKR     TO W-TIUTSKR                                     
016300                                                                          
016400     PERFORM IMS-GHU-WDF411                                               
016500     IF SEGMENT-FINNS                                                     
016600        PERFORM IMS-DLET-WDF411                                           
016700     END-IF                                                               
016800     .                                                                    
016900                                                                          
017000 Z-FINIT SECTION.                                                         
017100                                                                          
017200                                                                          
017300     CLOSE W4639R                                                         
017400     SKIP2                                                                
017500     MOVE 'S' TO POSTSUM-OPKOD                                            
017600     CALL POSTSUM USING POSTSUM-PARM                                      
017700     .                                                                    
017800     EJECT                                                                
017900 S01-LAES-W4639R  SECTION.                                                
018000     SKIP2                                                                
018100     READ W4639R INTO IN-AREA                                             
018200     AT END                                                               
018300        MOVE HIGH-VALUE TO IN-AREA                                        
018400        SET END-OF-W4639R TO TRUE                                         
018500                                                                          
018600     NOT AT END                                                           
018700        MOVE 'W4639R' TO POSTSUM-FDNAMN                                   
018800        MOVE 'W46391D1' TO POSTSUM-DDNAMN2                                
018900*       MOVE IN-IDPTYP TO POSTSUM-TRANSTYP                                
019000        CALL POSTSUM USING POSTSUM-PARM                                   
019100                                                                          
019200        ADD 1 TO W-W4639R-KVPOST-IN                                       
019300     END-READ                                                             
019400     .                                                                    
019500     EJECT                                                                
019600 X-TAG-CHECKPOINT   SECTION.                                              
019700                                                                          
019800     PERFORM IMS-CHECKPOINT                                               
019900     MOVE ZERO TO CHKP-ANT                                                
020000     .                                                                    
020100     EJECT                                                                
020200* --- IMS SEKTIONER ---                                                   
020300                                                                          
020400     EJECT                                                                
021600                                                                          
021700 IMS-GHU-WDF411 SECTION.                                                  
021900                                                                          
022200     STRING 'WDF401  (IDLEVNR  =' W-IDLEVNR-X ')'                         
022300           DELIMITED BY SIZE INTO SSA1                                    
022400     STRING 'WDF411  (WDF411KY =' W-WDF411KY-X ')'                        
022500          DELIMITED BY SIZE INTO SSA2                                     
022600     MOVE '  GE'              TO GODK-STATUSKODER                         
022601                                                                          
022700     CALL CBLTDLI USING GHU WDF4-PCB DLI-IO-WDF411 SSA1 SSA2              
022800     MOVE WDF4-STATUS-CODE    TO STATUS-WS                                
022900     PERFORM IMS-STATUSKONTROLL                                           
023000     .                                                                    
023100                                                                          
023200                                                                          
023300 IMS-DLET-WDF411 SECTION.                                                 
023500                                                                          
023700     MOVE '  '               TO GODK-STATUSKODER                          
023800     CALL CBLTDLI USING DLET WDF4-PCB DLI-IO-WDF411                       
023900     MOVE WDF4-STATUS-CODE   TO STATUS-WS                                 
024000     PERFORM IMS-STATUSKONTROLL                                           
024102     .                                                                    
024200                                                                          
024210                                                                          
024220 IMS-RESTART SECTION.                                                     
024230     SKIP2                                                                
024240     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
024250     MOVE '  ' TO GODK-STATUSKODER                                        
024260     CALL CBLTDLI USING XRST MSG-PCB                                      
024271                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
024280                        CHKP-AREA-LENGTH CHKP-AREA                        
024290     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
024291     PERFORM IMS-STATUSKONTROLL                                           
024292     .                                                                    
024293                                                                          
024300     SKIP3                                                                
024400 IMS-CHECKPOINT SECTION.                                                  
024500     SKIP2                                                                
024600     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
024700     MOVE '  XD' TO GODK-STATUSKODER                                      
024800     CALL CBLTDLI USING CHKP MSG-PCB                                      
024900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
025000                        CHKP-AREA-LENGTH CHKP-AREA                        
025100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
025200     PERFORM IMS-STATUSKONTROLL                                           
025300                                                                          
025400     IF IMS-EJ-OK                                                         
025500       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
025600       DISPLAY FELTEXT                                                    
025700       CALL FELLOG                                                        
025800     END-IF                                                               
025900     .                                                                    
026000     EJECT                                                                
026100 IMS-STATUSKONTROLL SECTION.                                              
026200     SKIP2                                                                
026300     SET STATUS-IX TO 1                                                   
026400     SEARCH GODK-STATUS                                                   
026500       AT END                                                             
026600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
026700           DELIMITED BY SIZE INTO FELTEXT                                 
026800         DISPLAY FELTEXT                                                  
026900         CALL FELLOG                                                      
027000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
027100         CONTINUE                                                         
027200     END-SEARCH                                                           
027300     .                                                                    
