000100 ID DIVISION.                                                             
000300 PROGRAM-ID.             W4635200.                                        
000400 AUTHOR.                 KERSTIN JOHANSSON  GUIDE DATAKONSULT AB          
000500 DATE-WRITTEN.           OKT 1990.                                        
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*            BMP, UPPLÄGGNING AV PACNINGSTRANSAR FÅN DIREKT-              
001100*            LEVERANTöR På DISPATCHERN.                                   
002500*            CHECKPOINT TAGES FÖR VARJE NY ORDER.                         
002700*                                                                         
002800*    DATABASER: UPPDATERAR   KOMMUNIKATIONS DB                            
002900*                            WLKOMA-(WDP8)                                
003000*               UPPDATERAR   HÄNDELSE REGISTER (CHKPOINT)                 
003100*                            WL4579-(WDGX)                                
003200*                                                                         
003300*    ABENDKODER:                                                          
003400*            U0999      - FELLOG                                          
003500     EJECT                                                                
003600 ENVIRONMENT DIVISION.                                                    
003700                                                                          
003800 INPUT-OUTPUT SECTION.                                                    
003900                                                                          
004000 FILE-CONTROL.                                                            
004100                                                                          
004200*                  INFIL: GODKÄNDA POSTER                                 
004400     SELECT  W46352                   ASSIGN TO    W46352D1.              
004500     SKIP3                                                                
004700 DATA DIVISION.                                                           
004800                                                                          
004900 FILE SECTION.                                                            
005000                                                                          
005100 FD  W46352                                                               
005200     LABEL RECORD STANDARD                                                
005300     RECORDING      F                                                     
005400     BLOCK CONTAINS 0.                                                    
005500                                                                          
005600*01  -COPY W46352 -L.                                                     
006000     EJECT                                                                
007600 WORKING-STORAGE SECTION.                                                 
007601                                                                          
007610*    -- CHECKED BY WY2000                                                 
007700 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W4635200'.            
007800 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
008100 77  MSG-IO-AREA-LENGTH-1        PIC S9(9)   VALUE +32  COMP SYNC.        
008200 77  MSG-IO-AREA-1               PIC X(32)   VALUE SPACE.                 
008300 77  CHKP-AREA-1-LENGTH          PIC S9(9)   VALUE +32  COMP SYNC.        
008400 77  CHKP-AREA-1                 PIC X(32)   VALUE SPACE.                 
008410 77  CHKP-RAKNARE                PIC S9(4)   COMP SYNC VALUE +20.         
008420 77  IDEX                        PIC S9(4)   COMP SYNC VALUE +0.          
008510 77  W-ANT-POSTER-FORBI          PIC S9(7)   COMP-3.                      
008511 77  W-ANT-POSTER                PIC S9(7)   COMP-3.                      
008600 77  JA                          PIC X       VALUE 'J'.                   
008700 77  NEJ                         PIC X       VALUE 'N'.                   
008800 77  W46352-EOF                  PIC X       VALUE 'N'.                   
009800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   VALUE +16 COMP SYNC.         
011300     EJECT                                                                
011400                                                                          
011410 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
011420 01  TIDPUNKT                    PIC 9(8)    VALUE ZERO.                  
011500*    ---- AREA FÖR INFIL W46352                                           
011600 01  FILLER                      PIC X(8)    VALUE 'IN-AREA'.             
011700 01  IN-AREA                     PIC X(1000).                             
011800*01  FILLER -COPY W46352 -PRE IN- -RED IN-AREA.                           
012300     EJECT                                                                
012400*01  FILLER -COPY W46336 -PRE IN2- -RED IN-AREA.                          
012500     EJECT                                                                
013400*    ---- SUBPROGRAM OCH PARAMETERAREOR                                   
013500 01  DYNAMISKA-SUBPROGRAM.                                                
013600   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
013700   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
013800   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
014000   03  W006KOM                   PIC X(8)    VALUE 'W006KOM '.            
014100   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
014400     EJECT                                                                
014500*01  FILLER -COPY W0005       -PRE POSTSUM-.                              
014700     EJECT                                                                
014800*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
014900*                                                                         
015000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015100     SKIP3                                                                
015200*    ---- STATUSKOD FRÅN IMS                                              
015300                                                                          
015400 01  STATUS-WS                   PIC XX.                                  
015800     88  IMS-EJ-OK                           VALUE 'XD'.                  
015810     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
015820     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015900     SKIP3                                                                
016000 01  GODK-STATUSKODER.                                                    
016100   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
016200     SKIP3                                                                
016300 01  SSA1                        PIC X(64).                               
016400 01  SSA2                        PIC X(64).                               
016500     EJECT                                                                
016600 01  FILLER                  PIC X(16)   VALUE 'NYCKLAR-TILL-DLI'.        
016700     SKIP3                                                                
017000*01  -COPY WDGX01                                                         
017010     EJECT                                                                
017100*    IMS FUNKTIONSKODER                                                   
017110*    -COPY W0003                                                          
017120     EJECT                                                                
017200 01  FILLER                  PIC X(16)   VALUE '4580-IO-AREA'.            
017300 01  4580-IO-AREA.                                                        
017500*03  FILLER  -COPY WDGX4580                                               
017700     EJECT                                                                
017800 01  FILLER                  PIC X(16)   VALUE 'MSG-KOM-AREA'.            
017900*01  -COPY WMSGKOM                                                        
018100     EJECT                                                                
018200                                                                          
018300 01  FILLER                  PIC X(16)   VALUE 'MSG-IO-AREA'.             
018400     SKIP3                                                                
018500*01  -COPY WMSGAREA                                                       
018700     EJECT                                                                
018800*                                                                         
018900*    --- AREOR FÖR W006KOM SUBMODUL                                       
019000*                                                                         
019100 01  FILLER                      PIC X(16)   VALUE 'KOM-IO-AREA'.         
019200 01  KOM-IO-AREA.                                                         
019300   03  KOM-AREA                     PIC X(1000) VALUE SPACE.              
019400*03  FILLER  -COPY W4I39901      -RED KOM-AREA.                           
019900     EJECT                                                                
019910*03  FILLER  -COPY W4I39902      -RED KOM-AREA.                           
019920     EJECT                                                                
020000                                                                          
020100 LINKAGE SECTION.                                                         
020200*01  -COPY W0009         -PRE MSG-                                        
020400     SKIP3                                                                
020500 01  DISP-PCB                PIC X.                                       
020600 01  KOMA-PCB                PIC X.                                       
020700     EJECT                                                                
021100*01  -COPY W0008         -PRE 4579-                                       
021300     05  FILLER              PIC X.                                       
021400     EJECT                                                                
021600 PROCEDURE DIVISION  USING                                                
021700                     MSG-PCB                                              
021800                     DISP-PCB                                             
021900                     KOMA-PCB                                             
022000                     4579-PCB.                                            
022100     ENTRY 'DLITCBL' USING                                                
022200                     MSG-PCB                                              
022300                     DISP-PCB                                             
022400                     KOMA-PCB                                             
022500                     4579-PCB.                                            
022600                                                                          
022700     PERFORM A-INITIERA                                                   
022710                                                                          
022720     PERFORM IMS-RESTART                                                  
022730     PERFORM IMS-LAS-ATERSTART                                            
022731                                                                          
022732     IF SEGMENT-SAKNAS                                                    
022733        MOVE SPACE        TO 4580-WDGX4580-CTX                            
022734        MOVE '1'          TO 4580-KDSEGKEY                                
022735        MOVE ZERO         TO 4580-KVPOST                                  
022736        MOVE DAGENS-DATUM TO 4580-TIUPPDAT                                
022737        MOVE TIDPUNKT     TO 4580-TIUPPTID                                
022738                                                                          
022739        PERFORM IMS-ISRT-ATERSTART                                        
022740        PERFORM IMS-LAS-ATERSTART                                         
022741     END-IF                                                               
022742                                                                          
022743     IF 4580-KVPOST > +0                                                  
022750        PERFORM B-LAES-FRAM-TILL-CHKPOINT                                 
022800     ELSE                                                                 
022810        PERFORM S01-LAS-W46352                                            
022820        MOVE +1 TO W-ANT-POSTER                                           
022900     END-IF                                                               
022950                                                                          
023000     PERFORM UNTIL W46352-EOF = JA                                        
023100                                                                          
023900        PERFORM C-BEARBETA                                                
023901                                                                          
023902        PERFORM S01-LAS-W46352                                            
023903        ADD  +1 TO W-ANT-POSTER                                           
023904                                                                          
023905        IF  W46352-EOF NOT = JA                                           
023906        AND W-ANT-POSTER > CHKP-RAKNARE                                   
023907            PERFORM C-TAG-CHECKPOINT                                      
023908            MOVE +1 TO W-ANT-POSTER                                       
023909        END-IF                                                            
023910     END-PERFORM                                                          
024000                                                                          
024100     PERFORM Z-FINIT                                                      
024200     MOVE ZERO TO RETURN-CODE                                             
024300     GOBACK                                                               
024400     .                                                                    
024500     EJECT                                                                
024600                                                                          
024700 A-INITIERA SECTION.                                                      
024800     SKIP2                                                                
025100     OPEN INPUT W46352                                                    
025200                                                                          
025300     ACCEPT DAGENS-DATUM  FROM DATE                                       
025310     ACCEPT TIDPUNKT      FROM TIME                                       
025400     MOVE SPACE                TO MSG-AREA                                
025700                                                                          
025800     MOVE PROGRAM-NAMN         TO POSTSUM-PROGNAMN                        
025810     .                                                                    
025820     EJECT                                                                
026000 B-LAES-FRAM-TILL-CHKPOINT SECTION.                                       
026100                                                                          
026200     MOVE +0                   TO W-ANT-POSTER-FORBI                      
026400     PERFORM S01-LAS-W46352                                               
026420                                                                          
026500     PERFORM UNTIL W46352-EOF     = JA                                    
026510                OR W-ANT-POSTER-FORBI = 4580-KVPOST                       
026520        PERFORM S01-LAS-W46352                                            
026610        ADD +1  TO W-ANT-POSTER-FORBI                                     
026700     END-PERFORM                                                          
026710                                                                          
026800     IF W46352-EOF = JA                                                   
026900        MOVE 'INPUTFIL EOF = JA, VID ÅTERSTART'                           
027000                      TO FELTEXT                                          
027100        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
027810     ELSE                                                                 
027811        MOVE +1  TO W-ANT-POSTER                                          
027820     END-IF                                                               
027900     .                                                                    
028000     EJECT                                                                
030600                                                                          
030700 C-BEARBETA SECTION.                                                      
030800                                                                          
036000     MOVE SPACE                  TO MSG-KOM-WMSGKOM                       
036010     MOVE +54                    TO MSG-KOM-KVLL                          
036020     MOVE LOW-VALUE              TO MSG-KOM-KDZ1                          
036030     MOVE LOW-VALUE              TO MSG-KOM-KDZ2                          
036040     MOVE SPACE                  TO MSG-KOM-KDTRANS                       
036041     IF IN-IDANSTNR = '01441'                                             
036042       MOVE 'W46336'             TO MSG-KOM-IDCPYTXT                      
036043     ELSE                                                                 
036044       MOVE 'W46352'             TO MSG-KOM-IDCPYTXT                      
036045     END-IF                                                               
036060     MOVE IN-IDSNDNOD            TO MSG-KOM-IDSNDNOD                      
036080     MOVE 'W4635200'             TO MSG-KOM-IDSNDJOB                      
036090     MOVE DAGENS-DATUM           TO MSG-KOM-TIREGDAT                      
036091     ADD  1                      TO TIDPUNKT                              
036092     MOVE TIDPUNKT               TO MSG-KOM-TIKLOCK                       
036093     MOVE SPACE                  TO MSG-KOM-IDMFSMED                      
036100                                                                          
036400     MOVE SPACE                  TO KOM-AREA                              
036500     MOVE +954                   TO MSG-KVLL                              
036600     MOVE LOW-VALUE              TO MSG-KDZ1                              
036700     MOVE LOW-VALUE              TO MSG-KDZ2                              
036800     MOVE 'W4T399X '             TO MSG-KDTRANS-1                         
036900     MOVE '4399'                 TO MSG-IDTRANS-1                         
037000     MOVE '2'                    TO MSG-KDMFSFOR-1                        
037010                                                                          
037020     MOVE IN-IDANSTNR            TO MID-IDANSTNR                          
037030     MOVE IN-IDDISTR             TO MID-IDDISTR                           
037040     MOVE IN-IDKUNDNR            TO MID-IDKUNDNR                          
037050     MOVE IN-IDORDNR             TO MID-IDORDNR                           
037051     MOVE IN-IDPRODNR            TO MID-IDPRODNR                          
037052     MOVE IN-IDDC                TO MID-IDDC                              
037060     MOVE IN-IDFAKT-GNB          TO MID-IDFAKT-GNB                        
037061                                                                          
037062     MOVE +1                     TO IDEX                                  
037063     IF IN-IDANSTNR = '01441'                                             
037064       PERFORM UNTIL IDEX > 15                                            
037065           MOVE IN2-IDRADNR  (IDEX) TO MID2-IDRADNR (IDEX)                
037066           MOVE IN2-KVLEVART (IDEX) TO MID2-KVLEVART (IDEX)               
037067           MOVE IN2-IDKLIENT (IDEX) TO MID2-IDKLIENT (IDEX)               
037068           MOVE IN2-IDARBREF (IDEX) TO MID2-IDARBREF (IDEX)               
037069           MOVE IN2-IDBIL    (IDEX) TO MID2-IDBIL    (IDEX)               
037070           MOVE IN2-IDVIN    (IDEX) TO MID2-IDVIN    (IDEX)               
037071           ADD +1                TO IDEX                                  
037072       END-PERFORM                                                        
037073     ELSE                                                                 
037074       PERFORM UNTIL IDEX > 90                                            
037075           MOVE IN-IDRADNR (IDEX) TO MID-IDRADNR (IDEX)                   
037076           MOVE IN-KVLEVART (IDEX) TO MID-KVLEVART (IDEX)                 
037077           ADD +1                TO IDEX                                  
037078       END-PERFORM                                                        
037080     END-IF                                                               
037116                                                                          
037117     MOVE KOM-AREA               TO MSG-INDATA-MINUS-1-TRANSKOD           
037118     CALL W006KOM USING MSG-PCB                                           
037119                        DISP-PCB                                          
037120                        KOMA-PCB                                          
037121                        MSG-KOM-WMSGKOM                                   
037122                        MSG-IO-AREA                                       
037123     IF MSG-KOM-IDMFSMED NOT = SPACE                                      
037124*       FELAKTIG UPPDATERING PÅ KOMMUNIKATIONS DB                         
037125*       DUBBLETT ELLER DATUM,TID EJ NUM - FÅR EJ INTRÄFFA                 
037126        MOVE ' FELAKTIG DATUM,TID PÅ INPUTFIL W46352 '                    
037127                      TO FELTEXT                                          
037128        DISPLAY ' FELAKTIG DATUM,TID INPUTFIL W46352 '                    
037129        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
037130     END-IF                                                               
037131                                                                          
037132     MOVE SPACE       TO KOM-AREA                                         
037133                                                                          
037140     .                                                                    
037200     EJECT                                                                
037300                                                                          
037310 C-TAG-CHECKPOINT SECTION.                                                
037320     SKIP2                                                                
037330*    UPPDATERA ÅTERSTARTREGISTRET                                         
037340     PERFORM IMS-LAS-ATERSTART                                            
037350     ADD  CHKP-RAKNARE   TO 4580-KVPOST                                   
037360     ACCEPT 4580-TIUPPDAT FROM DATE                                       
037370     ACCEPT 4580-TIUPPTID FROM TIME                                       
037380                                                                          
037390     PERFORM IMS-REPL-ATERSTART                                           
037391                                                                          
037392*    TAG CHECKPOINT                                                       
037393     PERFORM IMS-CHECKPOINT                                               
037394     .                                                                    
048300     EJECT                                                                
051300                                                                          
058200 Z-FINIT    SECTION.                                                      
058300     SKIP2                                                                
058500                                                                          
058600     CLOSE  W46352                                                        
058700                                                                          
058800*    NOLLA ÅTERSTARTINFORMATIONEN                                         
058900     PERFORM IMS-LAS-ATERSTART                                            
059000     MOVE +0                   TO 4580-KVPOST                             
059300     ACCEPT 4580-TIUPPDAT FROM DATE                                       
059400     ACCEPT 4580-TIUPPTID FROM TIME                                       
059410                                                                          
059500     PERFORM IMS-REPL-ATERSTART                                           
060400                                                                          
061500     MOVE 'S'      TO POSTSUM-OPKOD                                       
061600     CALL POSTSUM USING POSTSUM-PARM                                      
061700     .                                                                    
061800     EJECT                                                                
061900                                                                          
062000 S01-LAS-W46352 SECTION.                                                  
062100     SKIP2                                                                
062200     READ W46352 INTO IN-AREA                                             
062300       AT END                                                             
062400          MOVE JA TO W46352-EOF                                           
062500     END-READ                                                             
062600                                                                          
062700     IF W46352-EOF = NEJ                                                  
062800        MOVE 'W46352'       TO POSTSUM-FDNAMN                             
062900        MOVE 'W46352D1'     TO POSTSUM-DDNAMN2                            
063000        MOVE SPACE          TO POSTSUM-TRANSTYP                           
063100        CALL POSTSUM USING POSTSUM-PARM                                   
063200     END-IF                                                               
063300     .                                                                    
066000                                                                          
066029     EJECT                                                                
066030                                                                          
066100* IMS SECTIONER                                                           
066200     SKIP3                                                                
066300                                                                          
066400 IMS-RESTART SECTION.                                                     
066500     SKIP2                                                                
066600     MOVE SPACE TO MSG-IO-AREA-1                                          
066700     MOVE '  ' TO GODK-STATUSKODER                                        
066800     CALL CBLTDLI USING XRST MSG-PCB                                      
066900                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
067000                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
067100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
067200     PERFORM IMS-STATUSKONTROLL                                           
067300     .                                                                    
067400                                                                          
067500 IMS-CHECKPOINT SECTION.                                                  
067600     MOVE PROGRAM-NAMN TO MSG-IO-AREA-1                                   
067700     MOVE '  XD'       TO GODK-STATUSKODER                                
067800     CALL CBLTDLI USING CHKP MSG-PCB                                      
067900                        MSG-IO-AREA-LENGTH-1 MSG-IO-AREA-1                
068000                        CHKP-AREA-1-LENGTH CHKP-AREA-1                    
068100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
068200     PERFORM IMS-STATUSKONTROLL                                           
068300     IF IMS-EJ-OK                                                         
068400       DISPLAY 'IMS-KONTROLLREGION EJ TILLGÄNGLIG'                        
068500       MOVE ' IMS-KONTROLLREGION EJ TILLGÄNGLIG '                         
068600                            TO FELTEXT                                    
068700       CALL FELLOG                                                        
068800     END-IF                                                               
068900     .                                                                    
069000     EJECT                                                                
069100 IMS-LAS-ATERSTART SECTION.                                               
069200     SKIP2                                                                
069400     MOVE '4579'         TO IDHTYP                                        
069500     MOVE LOW-VALUE      TO NYCKEL-VALFRI                                 
069510     MOVE 'W4635200'     TO NYCKEL-VALFRI(1:8)                            
069600     STRING 'WL457901(WDGXKEY  =' WDGX01 ')'                              
069700                    DELIMITED BY SIZE INTO SSA1                           
069800     MOVE 'WL457911 '    TO SSA2                                          
069900     MOVE '  GE'           TO GODK-STATUSKODER                            
070000     CALL CBLTDLI USING GHU 4579-PCB 4580-IO-AREA SSA1 SSA2               
070100     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
070200     PERFORM IMS-STATUSKONTROLL                                           
070300     .                                                                    
070400                                                                          
070410 IMS-ISRT-ATERSTART SECTION.                                              
070420     SKIP2                                                                
070430     MOVE '4579'         TO IDHTYP                                        
070440     MOVE LOW-VALUE      TO NYCKEL-VALFRI                                 
070450     MOVE 'W4635200'     TO NYCKEL-VALFRI(1:8)                            
070460     STRING 'WL457901(WDGXKEY  =' WDGX01 ')'                              
070470                    DELIMITED BY SIZE INTO SSA1                           
070480     MOVE 'WL457911 '    TO SSA2                                          
070490     MOVE '  '           TO GODK-STATUSKODER                              
070491     CALL CBLTDLI USING ISRT 4579-PCB 4580-IO-AREA SSA1 SSA2              
070492     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
070493     PERFORM IMS-STATUSKONTROLL                                           
070494     .                                                                    
070495                                                                          
070500 IMS-REPL-ATERSTART SECTION.                                              
070600     SKIP2                                                                
070700     MOVE '  '             TO GODK-STATUSKODER                            
070800     CALL CBLTDLI USING REPL 4579-PCB 4580-IO-AREA                        
070900     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
071000     PERFORM IMS-STATUSKONTROLL                                           
071100     .                                                                    
071300                                                                          
074900 IMS-STATUSKONTROLL SECTION.                                              
075000     SET STATUS-IX TO 1                                                   
075100     SEARCH GODK-STATUS                                                   
075200       AT END                                                             
075300         MOVE ' STATUSKOD FRÅN IMS EJ TILLÅTEN '                          
075400                            TO FELTEXT                                    
075500         CALL FELLOG                                                      
075600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
075700         CONTINUE                                                         
075800     END-SEARCH                                                           
075900     .                                                                    
