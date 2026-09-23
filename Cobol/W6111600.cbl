000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6111600.                                                
000300 AUTHOR.         HÅKAN BOHLIN.                                            
000400 DATE-WRITTEN.   20/03/16.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        UPDATES WDK212 WITH NETWEIGHT CHANGES                            
000900*        FROM KDP.                                                        
001000*                                                                         
001100                                                                          
001200 ENVIRONMENT DIVISION.                                                    
001300                                                                          
001400 INPUT-OUTPUT SECTION.                                                    
001500 FILE-CONTROL.                                                            
001600*          --- NETWEIGHT UPDATES FROM KDP                                 
001700     SELECT W61108                     ASSIGN TO W61116D1.                
001800     EJECT                                                                
001900                                                                          
002000 DATA DIVISION.                                                           
002100 FILE SECTION.                                                            
002200 FD  W61108                                                               
002300     RECORDING       F                                                    
002400     BLOCK CONTAINS  0.                                                   
002500                                                                          
002600*01  -COPY W61106      -L.                                                
002700     EJECT                                                                
002800                                                                          
002900 WORKING-STORAGE SECTION.                                                 
003000 77  IDPGM                       PIC X(8)    VALUE 'W6111600'.            
003100 01  CHKP-VAR.                                                            
003200     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
003300     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
003400     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
003500     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
003600     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
003700     03 CHKP-MAX                 PIC S9(3)   VALUE +800 COMP-3.           
003800     03 CHKP-TOT                 PIC S9(7)   VALUE ZERO.                  
003900 77  JA                          PIC X       VALUE 'J'.                   
004000 77  NEJ                         PIC X       VALUE 'N'.                   
004300 77  W61108-EOF-SW               PIC X       VALUE 'N'.                   
004400     88  END-OF-W61108                       VALUE 'J'.                   
004500                                                                          
004600 01  FELTEXT.                                                             
004700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
004900                                                                          
005000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005100 01  FILLER REDEFINES DAGENS-DATUM.                                       
005200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005700     EJECT                                                                
005800                                                                          
005900 01  DYNAMISKA-SUBPROGRAM.                                                
006000*                                                                         
006100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006400     EJECT                                                                
006500                                                                          
006600*    --- PARAMETRAR TILL POSTSUM                                          
006700*                                                                         
006800*01  -COPY W0005   -PRE  POSTSUM-                                         
006900     EJECT                                                                
007000                                                                          
007100 01  IN-AREA-START               PIC X(24)   VALUE                        
007200                                             'IN-AREA-START'.             
007300*01  AREA -COPY W61106     -PRE IN-                                       
007400*                                                                         
007500     EJECT                                                                
007600                                                                          
007700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007800 01  NYCKLAR-TILL-DLI.                                                    
007810     03  W-IDARTNR-X.                                                     
007820         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
007900     03  W-KDSEGKEY-X.                                                    
008000         05  FILLER              PIC X(1)    VALUE '1'.                   
008100         05  FILLER              PIC X(79)   VALUE LOW-VALUE.             
008600                                                                          
008700*    --- STATUS-KOD FRÅN IMS                                              
008800 01  STATUS-WS                   PIC XX.                                  
008900     88  SEGMENT-FINNS                       VALUE '  '.                  
009000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009300     88  IMS-EJ-OK                           VALUE 'XD'.                  
009400                                                                          
009500 01  GODK-STATUSKODER.                                                    
009600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009700                                                                          
009800 01  SSA1                        PIC X(64).                               
009900 01  SSA2                        PIC X(64).                               
010000     EJECT                                                                
010100                                                                          
010200*    --- IMS FUNKTIONSKODER                                               
010300*01  -COPY W0003                                                          
010400     EJECT                                                                
010500                                                                          
010600*    ---  DLI INPUT-OUTPUT AREA                                           
010700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK201'.                      
010800 01  DLI-IO-WDK201.                                                       
010900*    03  -COPY WDK201                                                     
011000     EJECT                                                                
011100                                                                          
011200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK212'.                      
011300 01  DLI-IO-WDK212.                                                       
011400*    03  -COPY WDK212                                                     
012500     EJECT                                                                
012600                                                                          
012700 LINKAGE SECTION.                                                         
012800*01  -COPY W0009   -PRE MSG-                                              
012900     EJECT                                                                
013000                                                                          
013100*01  -COPY W0008   -PRE WDK2-                                             
013200     05  FILLER                  PIC X.                                   
013700     EJECT                                                                
013800                                                                          
013900 PROCEDURE DIVISION  USING MSG-PCB                                        
014000                           WDK2-PCB.                                      
014100 MAIN SECTION.                                                            
014200     ENTRY 'DLITCBL' USING MSG-PCB                                        
014300                           WDK2-PCB.                                      
014400                                                                          
014500     PERFORM A-INIT                                                       
014700                                                                          
014800     PERFORM S01-LAS-W61108                                               
014900     PERFORM UNTIL END-OF-W61108                                          
015000       IF CHKP-ANT > CHKP-MAX                                             
015100         PERFORM X-TAG-CHECKPOINT                                         
015200       END-IF                                                             
015300                                                                          
015400       PERFORM B-BEARBETA                                                 
015500       PERFORM S01-LAS-W61108                                             
015600     END-PERFORM                                                          
015700                                                                          
015800     PERFORM Z-FINIT                                                      
015900                                                                          
016000     MOVE ZERO TO RETURN-CODE                                             
016100     GOBACK                                                               
016200     .                                                                    
016300     EJECT                                                                
016400                                                                          
016500 A-INIT SECTION.                                                          
016600     PERFORM IMS-RESTART                                                  
016800     OPEN INPUT W61108                                                    
017000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017010     ACCEPT DAGENS-DATUM FROM DATE                                        
017100     .                                                                    
019700     EJECT                                                                
019800                                                                          
019900 B-BEARBETA SECTION.                                                      
020000     MOVE IN-IDARTNR                    TO W-IDARTNR                      
020100     PERFORM IMS-GU-WDK201                                                
020200                                                                          
020300     IF SEGMENT-SAKNAS                                                    
020400       MOVE W-IDARTNR                   TO ARTM-IDARTNR                   
020600       PERFORM IMS-ISRT-WDK201                                            
020700       ADD +1                           TO CHKP-ANT                       
020701                                                                          
020710       MOVE '1'                         TO KDP-KDSEGKEY                   
020720       MOVE IN-KVANTAL                  TO KDP-KVANTAL                    
020730       MOVE DAGENS-DATUM                TO KDP-TIUPPDAT                   
020740       MOVE IN-VKART-NTO                TO KDP-VKART-NTO                  
020750       PERFORM IMS-ISRT-WDK212                                            
020760       ADD +1                           TO CHKP-ANT                       
020770     ELSE                                                                 
020780       PERFORM IMS-GHNP-WDK212                                            
020790       IF SEGMENT-SAKNAS                                                  
020791         MOVE '1'                       TO KDP-KDSEGKEY                   
020792         MOVE IN-KVANTAL                TO KDP-KVANTAL                    
020793         MOVE DAGENS-DATUM              TO KDP-TIUPPDAT                   
020794         MOVE IN-VKART-NTO              TO KDP-VKART-NTO                  
020795         PERFORM IMS-ISRT-WDK212                                          
020803         ADD +1                         TO CHKP-ANT                       
020797       ELSE                                                               
               IF IN-KVANTAL = KDP-KVANTAL AND                                  
                  IN-VKART-NTO = KDP-VKART-NTO                                  
                 CONTINUE                                                       
               ELSE                                                             
020798           MOVE IN-KVANTAL                TO KDP-KVANTAL                  
020799           MOVE DAGENS-DATUM              TO KDP-TIUPPDAT                 
020800           MOVE IN-VKART-NTO              TO KDP-VKART-NTO                
020801           PERFORM IMS-REPL-WDK212                                        
020803           ADD +1                         TO CHKP-ANT                     
               END-IF                                                           
020802       END-IF                                                             
020810     END-IF                                                               
020900                                                                          
022700     .                                                                    
022800     EJECT                                                                
022900                                                                          
023000 Z-FINIT SECTION.                                                         
023600     CLOSE W61108                                                         
023700                                                                          
023800     MOVE 'S' TO POSTSUM-OPKOD                                            
023900     CALL POSTSUM USING POSTSUM-PARM                                      
024000     .                                                                    
024100     EJECT                                                                
024200                                                                          
024300 S01-LAS-W61108  SECTION.                                                 
024400     READ W61108 INTO IN-AREA                                             
024500     AT END                                                               
024600        MOVE HIGH-VALUE   TO IN-W61106                                    
024700        SET END-OF-W61108 TO TRUE                                         
024800                                                                          
024900     NOT AT END                                                           
025000        MOVE 'W61108'     TO POSTSUM-FDNAMN                               
025100        MOVE 'W61116D1'   TO POSTSUM-DDNAMN2                              
025200        MOVE 'IN-'        TO POSTSUM-TRANSTYP                             
025300        CALL POSTSUM USING POSTSUM-PARM                                   
025600     END-READ                                                             
025700     .                                                                    
025800     EJECT                                                                
025900                                                                          
026000 X-TAG-CHECKPOINT   SECTION.                                              
026700     PERFORM IMS-CHECKPOINT                                               
026800     MOVE ZERO TO CHKP-ANT                                                
026900     .                                                                    
027000     EJECT                                                                
027100                                                                          
027200* --- IMS SEKTIONER ---                                                   
029600     EJECT                                                                
029700                                                                          
029800 IMS-GU-WDK201 SECTION.                                                   
029900     STRING 'WDK201  (IDARTNR  =' W-IDARTNR-X ')'                         
030000          DELIMITED BY SIZE INTO SSA1                                     
030100     MOVE '  GE'           TO GODK-STATUSKODER                            
030200     CALL CBLTDLI USING GU WDK2-PCB DLI-IO-WDK201 SSA1                    
030300     MOVE WDK2-STATUS-CODE TO STATUS-WS                                   
030400     PERFORM IMS-STATUSKONTROLL                                           
030500     .                                                                    
030600                                                                          
030700 IMS-ISRT-WDK201 SECTION.                                                 
030800     MOVE 'WDK201  '       TO SSA1                                        
030900     MOVE '  '             TO GODK-STATUSKODER                            
031000     CALL CBLTDLI USING ISRT WDK2-PCB DLI-IO-WDK201 SSA1                  
031100     MOVE WDK2-STATUS-CODE TO STATUS-WS                                   
031200     PERFORM IMS-STATUSKONTROLL                                           
031300     .                                                                    
031400                                                                          
031500 IMS-GHNP-WDK212 SECTION.                                                 
031600     MOVE 'WDK212  '       TO SSA1                                        
032000     MOVE '  GE'           TO GODK-STATUSKODER                            
032100     CALL CBLTDLI USING GHNP WDK2-PCB DLI-IO-WDK212 SSA1                  
032200     MOVE WDK2-STATUS-CODE TO STATUS-WS                                   
032300     PERFORM IMS-STATUSKONTROLL                                           
032400     .                                                                    
032500                                                                          
032600 IMS-ISRT-WDK212 SECTION.                                                 
032610     STRING 'WDK201  (IDARTNR  =' W-IDARTNR-X ')'                         
032620          DELIMITED BY SIZE INTO SSA1                                     
032710     MOVE 'WDK212  '       TO SSA2                                        
032800     MOVE '  '             TO GODK-STATUSKODER                            
032900     CALL CBLTDLI USING ISRT WDK2-PCB DLI-IO-WDK212 SSA1 SSA2             
033000     MOVE WDK2-STATUS-CODE TO STATUS-WS                                   
033100     PERFORM IMS-STATUSKONTROLL                                           
033200     .                                                                    
033300                                                                          
033400 IMS-REPL-WDK212 SECTION.                                                 
033500     MOVE '  '             TO GODK-STATUSKODER                            
033600     CALL CBLTDLI USING REPL WDK2-PCB DLI-IO-WDK212                       
033700     MOVE WDK2-STATUS-CODE TO STATUS-WS                                   
033800     PERFORM IMS-STATUSKONTROLL                                           
033900     .                                                                    
034000     EJECT                                                                
034100                                                                          
034200 IMS-RESTART SECTION.                                                     
034300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
034400     MOVE '  ' TO GODK-STATUSKODER                                        
034500     CALL CBLTDLI USING XRST MSG-PCB                                      
034600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
034700                        CHKP-AREA-LENGTH CHKP-AREA                        
034800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
034900     PERFORM IMS-STATUSKONTROLL                                           
035000     .                                                                    
035100                                                                          
035200 IMS-CHECKPOINT SECTION.                                                  
035300     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
035400     MOVE '  XD' TO GODK-STATUSKODER                                      
035500     CALL CBLTDLI USING CHKP MSG-PCB                                      
035600                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
035700                        CHKP-AREA-LENGTH CHKP-AREA                        
035800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
035900     PERFORM IMS-STATUSKONTROLL                                           
036000                                                                          
036100     IF IMS-EJ-OK                                                         
036200       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
036300       DISPLAY FELTEXT                                                    
036400       CALL FELLOG                                                        
036500     END-IF                                                               
036600     .                                                                    
036700     EJECT                                                                
036800                                                                          
036900 IMS-STATUSKONTROLL SECTION.                                              
037000     SET STATUS-IX TO 1                                                   
037100     SEARCH GODK-STATUS                                                   
037200       AT END                                                             
037300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
037400           DELIMITED BY SIZE INTO FELTEXT                                 
037500         DISPLAY FELTEXT                                                  
037600         CALL FELLOG                                                      
037700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
037800         CONTINUE                                                         
037900     END-SEARCH                                                           
038000     .                                                                    
