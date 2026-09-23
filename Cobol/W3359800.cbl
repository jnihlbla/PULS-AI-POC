001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     W3359800.                                                
001400 AUTHOR.         GAVIN SMITH.                                             
001500 DATE-WRITTEN.   02/10/16.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800                                                                          
001900*    FUNKTION:                                                            
002100*        RENSAR WDC7-POSTER SOM FINNS PÅ INFIL W33590(W335P090)           
002200*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003400     SKIP2                                                                
003410*          --- WDC7 POSTER SOM SKALL RENSAS                               
003420     SELECT W33590                     ASSIGN TO W33598D1.                
003430     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 FILE SECTION.                                                            
003800     SKIP2                                                                
003910 FD  W33590                                                               
003920     LABEL RECORD    STANDARD                                             
003930     RECORDING       F                                                    
003940     BLOCK CONTAINS  0.                                                   
003950     SKIP2                                                                
003960*01  -COPY W33590          -L.                                            
003970                                                                          
003980     EJECT                                                                
004000 WORKING-STORAGE SECTION.                                                 
004100                                                                          
004200 77  IDPGM                       PIC X(8)    VALUE 'W3359800'.            
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004410 01  CHKP-VAR.                                                            
004420     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004430     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004440     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004450     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004460     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004470     03 CHKP-MAX                 PIC S9(3)   VALUE +300 COMP-3.           
004500     SKIP2                                                                
004510 77  W33590-EOF-SW               PIC X       VALUE 'N'.                   
004520     88  END-OF-W33590                       VALUE 'J'.                   
004530     SKIP2                                                                
004600 01  FELTEXT.                                                             
004700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005300     EJECT                                                                
006000 01  DYNAMISKA-SUBPROGRAM.                                                
006100*                                                                         
006200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007000     EJECT                                                                
007001*    --- PARAMETRAR TILL POSTSUM                                          
007002*                                                                         
007003*01  -COPY W0005      -PRE  POSTSUM-                                      
007004     EJECT                                                                
007005 01  W33590-AREA-START           PIC X(24)   VALUE                        
007006                                             'W33590-AREA-START'.         
007007     SKIP2                                                                
007008                                                                          
007009*01  AREA -COPY W33590     -PRE IN-                                       
007010     EJECT                                                                
007100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007200     SKIP3                                                                
007300 01  NYCKLAR-TILL-DLI.                                                    
007401     03  W-WDC701KY-X.                                                    
007402         05  W-WDC701KY          PIC X(26)   VALUE SPACE.                 
007403     03  W-IDPRQUES-X.                                                    
007410         05  W-IDPRQUES          PIC 9(7)    VALUE ZERO.                  
007500     SKIP2                                                                
007600*    --- STATUS-KOD FRÅN IMS                                              
007700 01  STATUS-WS                   PIC XX.                                  
007800     88  SEGMENT-FINNS                       VALUE '  '.                  
008000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008200     88  IMS-EJ-OK                           VALUE 'XD'.                  
008300     SKIP2                                                                
008400 01  GODK-STATUSKODER.                                                    
008500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
008600     SKIP3                                                                
008700 01  SSA1                        PIC X(64).                               
008800 01  SSA2                        PIC X(64).                               
008900     EJECT                                                                
009000*    --- IMS FUNKTIONSKODER                                               
009100*01  -COPY W0003                                                          
009200     EJECT                                                                
009400*    ---  DLI INPUT-OUTPUT AREA                                           
009500                                                                          
009601 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC701'.                      
009602 01  DLI-IO-WDC701.                                                       
009603*    03  -COPY WDC701                                                     
009900     EJECT                                                                
009910 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDC711'.                      
009920 01  DLI-IO-WDC711.                                                       
009930*    03  -COPY WDC711                                                     
009940     EJECT                                                                
010000 LINKAGE SECTION.                                                         
010100                                                                          
010200*01  -COPY W0009   -PRE MSG-                                              
010301                                                                          
010302*01  -COPY W0008  -PRE WDC7-                                              
010310     05  FILLER                  PIC X.                                   
010600     EJECT                                                                
010701 PROCEDURE DIVISION  USING MSG-PCB WDC7-PCB.                              
010702 MAIN SECTION.                                                            
010710     ENTRY 'DLITCBL' USING MSG-PCB WDC7-PCB.                              
010800                                                                          
011000     SKIP2                                                                
011010     PERFORM A-INIT                                                       
011020                                                                          
011100     PERFORM S01-LAES-W33590                                              
011300     PERFORM UNTIL END-OF-W33590                                          
011310                                                                          
011400       IF IN-IDSEGM = 'WDC701'                                            
011500         MOVE IN-WDC701   TO W-WDC701KY                                   
011510         PERFORM IMS-GHU-WDC701                                           
011511         IF SEGMENT-FINNS                                                 
011512           PERFORM IMS-DLET-WDC701                                        
011513           ADD 1 TO CHKP-ANT                                              
011514         END-IF                                                           
011600       END-IF                                                             
011700                                                                          
011800       IF IN-IDSEGM = 'WDC711'                                            
011900         MOVE IN-WDC701   TO W-WDC701KY                                   
011910         MOVE IN-IDPRQUES TO W-IDPRQUES                                   
012000         PERFORM IMS-GHU-WDC711                                           
012001         IF SEGMENT-FINNS                                                 
012010           PERFORM IMS-DLET-WDC711                                        
012011           ADD 1 TO CHKP-ANT                                              
012012         END-IF                                                           
012013       END-IF                                                             
012019                                                                          
012020       IF CHKP-ANT > CHKP-MAX                                             
012021         PERFORM X-TAG-CHECKPOINT                                         
012022       END-IF                                                             
012023                                                                          
012040       PERFORM S01-LAES-W33590                                            
012220     END-PERFORM                                                          
012300                                                                          
012400     PERFORM Z-FINIT                                                      
012600     MOVE ZERO TO RETURN-CODE                                             
012700     GOBACK                                                               
012800     .                                                                    
012900     EJECT                                                                
012910 A-INIT SECTION.                                                          
012920                                                                          
012930     PERFORM IMS-RESTART                                                  
012940                                                                          
012950     OPEN INPUT W33590                                                    
012960                                                                          
012990     MOVE IDPGM                TO POSTSUM-PROGNAMN                        
012991     MOVE +0                   TO  CHKP-ANT                               
012992     .                                                                    
012993     EJECT                                                                
013000 X-TAG-CHECKPOINT   SECTION.                                              
013100                                                                          
013400     PERFORM IMS-CHECKPOINT                                               
013500     MOVE ZERO TO CHKP-ANT                                                
013700     .                                                                    
013800     EJECT                                                                
013900 Z-FINIT SECTION.                                                         
014000                                                                          
014100     CLOSE W33590                                                         
014200                                                                          
014300     MOVE 'S'                  TO POSTSUM-OPKOD                           
014400     CALL POSTSUM              USING POSTSUM-PARM                         
014500     .                                                                    
014600     EJECT                                                                
014700 S01-LAES-W33590  SECTION.                                                
014800                                                                          
014900     READ W33590 INTO IN-AREA                                             
015000     AT END                                                               
015100        SET END-OF-W33590      TO TRUE                                    
015200                                                                          
015300     NOT AT END                                                           
015310        MOVE 'W33590'          TO POSTSUM-FDNAMN                          
015320        MOVE 'W33590D1'        TO POSTSUM-DDNAMN2                         
015330        MOVE 'RENS'            TO POSTSUM-TRANSTYP                        
015350        CALL POSTSUM           USING POSTSUM-PARM                         
015360     END-READ                                                             
015370     .                                                                    
015380     EJECT                                                                
015400* --- IMS SEKTIONER ---                                                   
015500                                                                          
015612 IMS-GHU-WDC701 SECTION.                                                  
015613                                                                          
015614     STRING 'WDC701  (WDC701KY =' W-WDC701KY-X ')'                        
015615          DELIMITED BY SIZE INTO SSA1                                     
015616     MOVE '  GE' TO GODK-STATUSKODER                                      
015617     CALL CBLTDLI USING GHU WDC7-PCB DLI-IO-WDC701 SSA1                   
015618     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
015619     PERFORM IMS-STATUSKONTROLL                                           
015620     .                                                                    
015621     SKIP3                                                                
015642 IMS-DLET-WDC701 SECTION.                                                 
015643                                                                          
015644     MOVE '  ' TO GODK-STATUSKODER                                        
015645     CALL CBLTDLI USING DLET WDC7-PCB DLI-IO-WDC701                       
015646     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
015647     PERFORM IMS-STATUSKONTROLL                                           
015648     .                                                                    
015650     EJECT                                                                
015660 IMS-GHU-WDC711 SECTION.                                                  
015670                                                                          
015671     STRING 'WDC701  (WDC701KY =' W-WDC701KY-X ')'                        
015672          DELIMITED BY SIZE INTO SSA1                                     
015680     STRING 'WDC711  (IDPRQUES =' W-IDPRQUES-X ')'                        
015690          DELIMITED BY SIZE INTO SSA2                                     
015700     MOVE '  GE' TO GODK-STATUSKODER                                      
015710     CALL CBLTDLI USING GHU WDC7-PCB DLI-IO-WDC711 SSA1 SSA2              
015720     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
015730     PERFORM IMS-STATUSKONTROLL                                           
015740     .                                                                    
015750     SKIP3                                                                
015760 IMS-DLET-WDC711 SECTION.                                                 
015770                                                                          
015780     MOVE '  ' TO GODK-STATUSKODER                                        
015790     CALL CBLTDLI USING DLET WDC7-PCB DLI-IO-WDC711                       
015791     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
015792     PERFORM IMS-STATUSKONTROLL                                           
015793     .                                                                    
015794     EJECT                                                                
015800 IMS-STATUSKONTROLL SECTION.                                              
015900     SKIP2                                                                
016000     SET STATUS-IX TO 1                                                   
016100     SEARCH GODK-STATUS                                                   
016200       AT END                                                             
016300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
016400           DELIMITED BY SIZE INTO FELTEXT                                 
016500         DISPLAY FELTEXT                                                  
016600         CALL FELLOG                                                      
016700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
016800         CONTINUE                                                         
016900     END-SEARCH                                                           
017000     .                                                                    
017100 IMS-RESTART SECTION.                                                     
017300     SKIP2                                                                
017500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
017700     MOVE '  ' TO GODK-STATUSKODER                                        
017900     CALL CBLTDLI USING XRST MSG-PCB                                      
018100                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
018300                        CHKP-AREA-LENGTH CHKP-AREA                        
018500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
018700     PERFORM IMS-STATUSKONTROLL                                           
018900     .                                                                    
019100     SKIP3                                                                
019300 IMS-CHECKPOINT SECTION.                                                  
019500     SKIP2                                                                
019700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
019800     MOVE '  XD' TO GODK-STATUSKODER                                      
020000     CALL CBLTDLI USING CHKP MSG-PCB                                      
020200                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
020400                        CHKP-AREA-LENGTH CHKP-AREA                        
020600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
020800     PERFORM IMS-STATUSKONTROLL                                           
021200     IF IMS-EJ-OK                                                         
021400       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
021600       DISPLAY FELTEXT                                                    
021800       CALL FELLOG                                                        
022000     END-IF                                                               
022200     .                                                                    
022400     EJECT                                                                
