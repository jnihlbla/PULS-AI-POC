000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4281500.                                                
000300 AUTHOR.         ÖSTRÖM ELEONOR.                                          
000400 DATE-WRITTEN.   08/08/11.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PROGRAMMET SORTERAR INFIL FRÅN W4281400 SÅ ATT UTSKRIFT          
001000*        SKER SORTERAT PÅ DC OCH LAND.                                    
001100*        PROGRAMMET INGÅR I RUTIN W428R1.PERIODENS RADER.                 
001200*        LÄSER IN ALLA W42811-FILER FRÅN VECKORUTIN W428V1 OCH            
001300*        SKICKAR RADER TILL DISTR. OCH PRINT VIA WZ01.                    
001400*                                                                         
001500*        E'TRACKER. 6785206  DATED 2008-06-18                             
001600*        E-TRACKER 10143271 CHINA WAREHOUSE PROJECT-1 2011-12-08          
001700*                                                                         
001710*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- PERIODENS INLAGDA RADER KOD 72 HOS LDC                     
002700     SELECT W42814                     ASSIGN TO W42815D1.                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  W42814                                                               
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600                                                                          
003700*01  -COPY W42814      -L.                                                
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100 77  IDPGM                       PIC X(8)    VALUE 'W4281500'.            
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400 77  SPAR-IDLANDX2               PIC X(2)    VALUE SPACE.                 
004410 77  SPAR-IDFTG                  PIC 9(2)    VALUE ZERO.                  
004500 77  WS-YYMMDDHHMM               PIC 9(10)   VALUE ZERO.                  
004600                                                                          
004700 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
004800 77  KDRC-DISPLAY                PIC Z(5).                                
004900     SKIP2                                                                
005000 01  FELTEXT.                                                             
006000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006200                                                                          
006300 77  W42814-EOF-SW               PIC X       VALUE 'N'.                   
006400     88  END-OF-W42814                       VALUE 'J'.                   
006500     EJECT                                                                
006600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006700 01  FILLER REDEFINES DAGENS-DATUM.                                       
006800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007100     EJECT                                                                
007101                                                                          
007102*01  -COPY WWIDFTG                                                        
007110     EJECT                                                                
007120                                                                          
007200 01  DYNAMISKA-SUBPROGRAM.                                                
007300*                                                                         
007400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007800     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
007900     EJECT                                                                
008000*    --- PARAMETRAR TILL POSTSUM                                          
008100*                                                                         
008200*01  -COPY W0005   -PRE  POSTSUM-                                         
008300     EJECT                                                                
008400*    --- PARAMETERS TO ABEND                                              
008500                                                                          
008600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008900                                                                          
009000     EJECT                                                                
009100                                                                          
009200 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
009300*01  -COPY WZ01SEND                                                       
009400                                                                          
009500     EJECT                                                                
009600 01  FILLER                      PIC X(16)   VALUE 'HDR-AREA'.            
009700 01  HDR-AREA.                                                            
009800*    03  -COPY WZ01REQU  -PRE HDR-                                        
009900*    03  -COPY WZ04HDR                                                    
010000*                                                                         
010100 01  FILLER                      PIC X(16)   VALUE 'DOC-AREA'.            
010200 01  DOC-AREA.                                                            
010300*    03  -COPY W428131                                                    
010400*                                                                         
010500     EJECT                                                                
010600     EJECT                                                                
010700 01  IN-AREA-START               PIC X(24)   VALUE                        
010800                                             'IN-AREA-START'.             
010900     SKIP2                                                                
011000                                                                          
011100*01  AREA -COPY W42814     -PRE IN-                                       
011200*                                                                         
011300     EJECT                                                                
011400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011500     SKIP3                                                                
011600*    --- STATUS-KOD FRÅN IMS                                              
011700 01  STATUS-WS                   PIC XX.                                  
011800     88  SEGMENT-FINNS                       VALUE '  '.                  
011900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
012200     88  IMS-EJ-OK                           VALUE 'XD'.                  
012300     SKIP2                                                                
012400 01  GODK-STATUSKODER.                                                    
012500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012600     SKIP3                                                                
012700 01  SSA1                        PIC X(64).                               
012800 01  SSA2                        PIC X(64).                               
012900     EJECT                                                                
013000     EJECT                                                                
014000 LINKAGE SECTION.                                                         
014100                                                                          
014200*01  -COPY W0009   -PRE MSG-                                              
014300     EJECT                                                                
014400 01  DISTRDOC-PCB                PIC X.                                   
014500     EJECT                                                                
014600                                                                          
014700 PROCEDURE DIVISION  USING MSG-PCB DISTRDOC-PCB.                          
014800 MAIN SECTION.                                                            
014900     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB.                          
015000                                                                          
015100     SKIP2                                                                
015200     PERFORM A-INIT                                                       
015300                                                                          
015400     PERFORM S01-LAES-W42814                                              
015500                                                                          
015600     PERFORM UNTIL END-OF-W42814                                          
015700       PERFORM S05-OPEN-DAP-SEND                                          
015710       MOVE IN-IDFTG TO WS-IDFTG                                          
015720       IF IDFTG-PV                                                        
015730       OR IDFTG-CN                                                        
015740       OR IDFTG-IN                                                        
015750       OR IDFTG-KR                                                        
015760       OR IDFTG-TR                                                        
015770       OR IDFTG-BR                                                        
015780       OR IDFTG-MX                                                        
015790       OR IDFTG-ZA                                                        
015791       OR IDFTG-MY                                                        
015792       OR IDFTG-TH                                                        
015793       OR IDFTG-TW                                                        
015800         PERFORM S02-FLYTTA-HEADER-DATA                                   
015940       END-IF                                                             
015950       PERFORM S06-PUT-DAP-HEADER                                         
016000                                                                          
016100       MOVE IN-IDLANDX2   TO SPAR-IDLANDX2                                
016110       MOVE IN-IDFTG      TO SPAR-IDFTG                                   
016200                                                                          
016300       PERFORM UNTIL END-OF-W42814 OR (IN-IDFTG NOT = SPAR-IDFTG)         
016400                                                                          
016500           IF IN-IDLANDX2 NOT = SPAR-IDLANDX2                             
016600              MOVE ALL '+'      TO DOC-AREA                               
016700              MOVE 'LINE'       TO DOC-IDAFPRCD                           
016800              PERFORM S07-PUT-DOC                                         
016900              MOVE IN-IDLANDX2  TO SPAR-IDLANDX2                          
017000           END-IF                                                         
018000                                                                          
018100           PERFORM C-FLYTTA-DATA                                          
018200           PERFORM S07-PUT-DOC                                            
018300                                                                          
018400           PERFORM S01-LAES-W42814                                        
018500                                                                          
018600       END-PERFORM                                                        
018700                                                                          
018800       PERFORM S08-CLOSE-DAP-SEND                                         
018900                                                                          
019000     END-PERFORM                                                          
019100     PERFORM Z-FINIT                                                      
019200                                                                          
019300     MOVE ZERO TO RETURN-CODE                                             
019400     GOBACK                                                               
019500     .                                                                    
019600     EJECT                                                                
019700 A-INIT SECTION.                                                          
019800     SKIP2                                                                
019900                                                                          
020000     OPEN INPUT W42814                                                    
020100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020200                                                                          
020300     .                                                                    
020400     EJECT                                                                
020500 C-FLYTTA-DATA  SECTION.                                                  
020600                                                                          
020700     MOVE 'LINE'              TO DOC-IDAFPRCD                             
020800     MOVE IN-IDDC-RET         TO DOC-IDDC                                 
020900     MOVE IN-TIAAPP           TO DOC-TIAAPP                               
021000     MOVE IN-ADCITY           TO DOC-ADCITY                               
022000     MOVE IN-KVRETINL         TO DOC-KVRETINL                             
023000     MOVE IN-KVRETINL-SKR     TO DOC-KVRETINL-SKR                         
024000     MOVE IN-KVAVV-KVANT      TO DOC-KVAVV-KVANT                          
025000     MOVE IN-KVDAGDEC         TO DOC-KVDAGDEC                             
026000                                                                          
026100     .                                                                    
026200     EJECT                                                                
026300 Z-FINIT SECTION.                                                         
026400                                                                          
026500                                                                          
026600     CLOSE W42814                                                         
026700     SKIP2                                                                
026800     MOVE 'S' TO POSTSUM-OPKOD                                            
026900     CALL POSTSUM USING POSTSUM-PARM                                      
027000     .                                                                    
027100     EJECT                                                                
027200 S01-LAES-W42814  SECTION.                                                
027300     SKIP2                                                                
027400     READ W42814 INTO IN-AREA                                             
027500     AT END                                                               
027600        MOVE HIGH-VALUE TO IN-W42814                                      
027700        SET END-OF-W42814 TO TRUE                                         
027800                                                                          
027900     NOT AT END                                                           
028000        MOVE 'W42814'   TO POSTSUM-FDNAMN                                 
028100        MOVE 'W42815D1' TO POSTSUM-DDNAMN2                                
028200        MOVE SPACE      TO POSTSUM-TRANSTYP                               
028300        CALL POSTSUM USING POSTSUM-PARM                                   
028400     END-READ                                                             
028500     .                                                                    
028600     EJECT                                                                
028700 S02-FLYTTA-HEADER-DATA  SECTION.                                         
028800                                                                          
028900     MOVE 001             TO HDR-REQU-IDMSGVER                            
029000     MOVE SPACE           TO HDR-REQU-KDPGMACT                            
029100     MOVE 'W42815'        TO HDR-REQU-IDUSER                              
029200                                                                          
029300     MOVE 'MANDISC72PERIOD'   TO HDR-IDOUTTYPE                            
029400     MOVE SPACE               TO HDR-IDOUTREC                             
029500     MOVE 'EUW42815'          TO HDR-IDOUTREC                             
029510     EVALUATE TRUE                                                        
029511     WHEN IDFTG-CN                                                        
029512         MOVE 'CN'            TO HDR-IDOUTREC(1:2)                        
029513     WHEN IDFTG-IN                                                        
029514         MOVE 'AS'            TO HDR-IDOUTREC(1:2)                        
029515     WHEN IDFTG-KR                                                        
029516         MOVE 'KR'            TO HDR-IDOUTREC(1:2)                        
029517     WHEN IDFTG-TR                                                        
029518         MOVE 'TR'            TO HDR-IDOUTREC(1:2)                        
029519     WHEN IDFTG-BR                                                        
029520         MOVE 'BR'            TO HDR-IDOUTREC(1:2)                        
029521     WHEN IDFTG-MX                                                        
029522         MOVE 'MX'            TO HDR-IDOUTREC(1:2)                        
029523     WHEN IDFTG-ZA                                                        
029524         MOVE 'ZA'            TO HDR-IDOUTREC(1:2)                        
029525     WHEN IDFTG-MY                                                        
029526         MOVE 'MY'            TO HDR-IDOUTREC(1:2)                        
029527     WHEN IDFTG-TH                                                        
029528         MOVE 'TH'            TO HDR-IDOUTREC(1:2)                        
029529     WHEN IDFTG-TW                                                        
029530         MOVE 'TW'            TO HDR-IDOUTREC(1:2)                        
029540     END-EVALUATE                                                         
029600                                                                          
029700     MOVE FUNCTION CURRENT-DATE (3:10) TO WS-YYMMDDHHMM                   
029800     MOVE WS-YYMMDDHHMM       TO HDR-IDLIST                               
029900     .                                                                    
030000     EJECT                                                                
031010 S05-OPEN-DAP-SEND SECTION.                                               
031100*    MOVE 'S05-OPEN-DAP-S' TO CURR-SECTION                                
031200                                                                          
031300     MOVE 'CARPARTS.DAP.DISTRDOCWEB' TO SEND-ADDISPABS                    
031400     MOVE 'OPEN'                     TO SEND-KDFUNC                       
031500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
031600                         SEND-OPEN-AREA                                   
031700     IF SEND-KDRC > ZERO                                                  
031800       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
031900       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
032000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
032100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
032200     END-IF                                                               
032300     .                                                                    
032400     EJECT                                                                
032500                                                                          
032600 S06-PUT-DAP-HEADER SECTION.                                              
032700*    MOVE 'S06-PUT-DAP-HE' TO CURR-SECTION                                
032800                                                                          
032900     MOVE 'PUT'                           TO SEND-KDFUNC                  
033000     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
033100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
033200                         SEND-KVDLEN                                      
033300                         HDR-AREA                                         
033400     IF SEND-KDRC > ZERO                                                  
033500       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
033600       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
033700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
033800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
033900     END-IF                                                               
034000     .                                                                    
034100 S07-PUT-DOC      SECTION.                                                
034200*    MOVE 'S07-PUT-DOC ' TO CURR-SECTION                                  
034300                                                                          
034400     MOVE 'PUT'                           TO SEND-KDFUNC                  
034500     MOVE LENGTH OF DOC-AREA              TO SEND-KVDLEN                  
034600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
034700                         SEND-KVDLEN                                      
034800                         DOC-AREA                                         
034900     IF SEND-KDRC > ZERO                                                  
035000       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
035100       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
035200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
035300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
035400     END-IF                                                               
035500     .                                                                    
035600     EJECT                                                                
035700 S08-CLOSE-DAP-SEND SECTION.                                              
035800*    MOVE 'S08-CLOSE-DAP-' TO CURR-SECTION                                
035900                                                                          
036000     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
036100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
036200                                                                          
036300     IF SEND-KDRC > 0                                                     
036400       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
036500       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
036600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
036700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
036800     END-IF                                                               
036900     .                                                                    
037000     EJECT                                                                
038000                                                                          
038170 IMS-STATUSKONTROLL SECTION.                                              
038200     SKIP2                                                                
038300     SET STATUS-IX TO 1                                                   
038400     SEARCH GODK-STATUS                                                   
038500       AT END                                                             
038600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
038700           DELIMITED BY SIZE INTO FELTEXT                                 
038800         DISPLAY FELTEXT                                                  
038900         CALL FELLOG                                                      
039000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
039100         CONTINUE                                                         
039200     END-SEARCH                                                           
039300     .                                                                    
