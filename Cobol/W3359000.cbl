000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3359000.                                                
000300 AUTHOR.         OLGRENER LASSI.                                          
000400 DATE-WRITTEN.   03/09/24.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        SKAPAR WDC7-RENSNINGPOSTER TILL W33598-BMP                       
000900*                                                                         
000910*        TILLÄGG 2008-02.RENSA BORT POSTER SOM LIGGER KVAR PGA FEL        
000920*        I ORDER/SKEPPNINGSSYSTEMET. GAMLA PRISFRÅGOR....KOLLAR           
000930*        ÄVEN MOT WDQ2 SÅ ATT ORDERN SAKNAS DÄR.                          
000940*                                                                         
000960*        PROGRAMMET LÄSER      WDC7   PRISFRÅGOR                          
000970*        PROGRAMMET LÄSER      WDQ2C  ORDERHUVUD                          
001000*                                                                         
001100*    ABENDKODER:                                                          
001200*        U0016 -  . . . .                                                 
001300*        U1000 -  . . . .                                                 
001400*                                                                         
001500*    E'TRACKER 6277114  DATED 20080207  RENSA FRÅGOR SOM FASTNAT.         
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- RENSNINGSPOSTER WDC7                                       
002600     SELECT W33590                     ASSIGN TO W33590D1.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP2                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  W33590                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500                                                                          
003600*01  POST -COPY W33590 -PRE  UT-  -L.                                     
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
004000 77  IDPGM                       PIC X(8)    VALUE 'W3359000'.            
004100 77  FELTEXT                     PIC X(32)   VALUE SPACE.                 
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004400     SKIP2                                                                
004500 01  PREV-ROOT-DLET-SW           PIC X       VALUE 'N'.                   
004600     88  PREV-ROOT-DLET                      VALUE 'J'.                   
004700     SKIP2                                                                
004800 01  DYNAMISKA-SUBPROGRAM.                                                
004900*                                                                         
005000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005300     EJECT                                                                
005400*    --- PARAMETRAR TILL POSTSUM                                          
005500*                                                                         
005600*01  -COPY W0005   -PRE  POSTSUM-                                         
005700     EJECT                                                                
005800 01  UT-AREA-START               PIC X(24)   VALUE                        
005900                                 'UT-AREA-START  '.                       
006000     SKIP2                                                                
006100 01  UT-AREA.                                                             
006200*    03  -COPY W33590  -PRE UT-                                           
006300     EJECT                                                                
006400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
006500     SKIP2                                                                
006510 01  NYCKLAR-TILL-DLI.                                                    
006520                                                                          
006530     03 W-WDQ2C1KY-X.                                                     
006540        05  W-SEQC-IDDISTR      PIC S9(5)   VALUE +0 COMP-3.              
006550        05  W-SEQC-IDKUNDNR     PIC S9(7)   VALUE +0 COMP-3.              
006560        05  W-SEQC-IDKUNDRF.                                              
006570          07  W-SEQC-IDORDNR7   PIC 9(7)    VALUE ZERO.                   
006580          07  FILLER            PIC X(3)    VALUE SPACE.                  
006590                                                                          
006591     EJECT                                                                
006592                                                                          
006600*    --- STATUS-KOD FRÅN IMS                                              
006700 01  STATUS-WS                   PIC XX.                                  
006800     88  SEGMENT-FINNS                       VALUE '  '.                  
006900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
006910     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
007000     SKIP2                                                                
007100 01  GODK-STATUSKODER.                                                    
007200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
007300     SKIP3                                                                
007400 01  SSA1                        PIC X(64).                               
007500     EJECT                                                                
007600*    --- IMS FUNKTIONSKODER                                               
007700*01  -COPY W0003                                                          
007800     EJECT                                                                
007900*    ---  DLI INPUT-OUTPUT AREA                                           
008000 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA'.           
008100 01  DLI-IO-AREA.                                                         
008200     03  IO-AREA                 PIC X(250).                              
008300*    03  -COPY WDC701 -RED IO-AREA                                        
008400*    03  -COPY WDC711 -RED IO-AREA                                        
008410     EJECT                                                                
008420                                                                          
008430 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDQ2C1'.         
008440                                                                          
008450 01  DLI-IO-WDQ2C1.                                                       
008460*    03  WDQ2C1 -COPY WDQ2C1                                              
008500     EJECT                                                                
008600 LINKAGE SECTION.                                                         
008700                                                                          
008800                                                                          
008900*01  -COPY W0008  -PRE WDC7-                                              
009000     05  FILLER                  PIC X.                                   
009010     EJECT                                                                
009020*01  -COPY W0008  -PRE WDQ2C-                                             
009030     05  FILLER                  PIC X.                                   
009100     EJECT                                                                
009200 PROCEDURE DIVISION  USING WDC7-PCB WDQ2C-PCB.                            
009300 MAIN SECTION.                                                            
009400     ENTRY 'DLITCBL' USING WDC7-PCB WDQ2C-PCB.                            
009500                                                                          
009600                                                                          
009700     PERFORM A-INIT                                                       
009800                                                                          
009900     PERFORM IMS-GET-WDC7                                                 
010000     PERFORM UNTIL SEGMENT-SLUT                                           
010100       EVALUATE WDC7-SEG-NAME-FB                                          
010200         WHEN 'WDC701'                                                    
010300           IF PREV-ROOT-DLET                                              
010400             MOVE 'WDC701' TO UT-IDSEGM                                   
010500             MOVE SPACE    TO UT-WDC711                                   
010600             PERFORM S11-SKRIV-W33590                                     
010700           END-IF                                                         
010800           MOVE PRQ-WDC701 TO UT-WDC701                                   
010900           MOVE JA         TO PREV-ROOT-DLET-SW                           
010910                                                                          
010930           MOVE PRQ-IDDISTR   TO W-SEQC-IDDISTR                           
010940           MOVE PRQ-IDKUNDNR  TO W-SEQC-IDKUNDNR                          
010950           MOVE PRQ-IDORDNR7  TO W-SEQC-IDORDNR7                          
011000                                                                          
011100         WHEN 'WDC711'                                                    
011200           MOVE NEJ           TO PREV-ROOT-DLET-SW                        
011300           IF LPRQ-FLALL = 'Y' OR 'J'                                     
011400             MOVE 'WDC711'    TO UT-IDSEGM                                
011500             MOVE LPRQ-WDC711 TO UT-WDC711                                
011600             PERFORM S11-SKRIV-W33590                                     
011700           END-IF                                                         
011710                                                                          
011720*- LAGT TILL LÄSNING FÖR ATT FÅ MED PRISFRÅGOR SOM FASTNAT AV             
011730*- OLIKA ORSAKER.(DUBLETTER, FEL I ORDER ETC.)20080207 E'T 6277114        
011800           IF LPRQ-FLALL = 'N' AND                                        
011810             LPRQ-DADATTID-REG > 0 AND                                    
011860             LPRQ-DADATTID-SVAR > 0 AND                                   
011870             LPRQ-DADATTID-OK > 0 AND                                     
011880             LPRQ-KDFEL = 0                                               
011890                                                                          
011891             IF LPRQ-ADDISPABS(1:22) = 'CARPARTS.PULS.CREPRICE'           
011892               CONTINUE                                                   
011893             ELSE                                                         
011894                                                                          
011895               PERFORM IMS-01-GU-WDQ2C1                                   
011896                                                                          
011897               IF SEGMENT-SAKNAS                                          
011898                 MOVE 'WDC711'    TO UT-IDSEGM                            
011899                 MOVE LPRQ-WDC711 TO UT-WDC711                            
011900                 PERFORM S11-SKRIV-W33590                                 
011901               END-IF                                                     
011902             END-IF                                                       
011903           END-IF                                                         
011904                                                                          
011910       END-EVALUATE                                                       
012000       PERFORM IMS-GET-WDC7                                               
012100     END-PERFORM                                                          
012200                                                                          
012300     IF PREV-ROOT-DLET                                                    
012400       MOVE 'WDC701' TO UT-IDSEGM                                         
012500       PERFORM S11-SKRIV-W33590                                           
012600     END-IF                                                               
012700                                                                          
012800     PERFORM Z-FINIT                                                      
012900                                                                          
013000     MOVE ZERO TO RETURN-CODE                                             
013100     GOBACK                                                               
013200     .                                                                    
013300     EJECT                                                                
013400 A-INIT SECTION.                                                          
013500                                                                          
013600     OPEN OUTPUT W33590                                                   
013700                                                                          
013800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
013900     .                                                                    
014000     EJECT                                                                
014100 Z-FINIT SECTION.                                                         
014200     CLOSE W33590                                                         
014300     SKIP2                                                                
014400     MOVE 'S' TO POSTSUM-OPKOD                                            
014500     CALL POSTSUM USING POSTSUM-PARM                                      
014600     .                                                                    
014700     EJECT                                                                
014800 S11-SKRIV-W33590 SECTION.                                                
014900                                                                          
015000     WRITE UT-POST FROM UT-AREA                                           
015100                                                                          
015200     MOVE 'RENS'     TO POSTSUM-TRANSTYP                                  
015300     MOVE 'W33590'   TO POSTSUM-FDNAMN                                    
015400     MOVE 'W33590D1' TO POSTSUM-DDNAMN2                                   
015500     CALL POSTSUM USING POSTSUM-PARM                                      
015600     .                                                                    
015700     EJECT                                                                
015800* --- IMS SEKTIONER ---                                                   
015900                                                                          
016000                                                                          
016100 IMS-GET-WDC7   SECTION.                                                  
016200                                                                          
016300     CALL CBLTDLI USING GN WDC7-PCB DLI-IO-AREA                           
016400     MOVE WDC7-STATUS-CODE TO STATUS-WS                                   
016500     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
016600     PERFORM IMS-STATUSKONTROLL                                           
016700     .                                                                    
016800     EJECT                                                                
016810 IMS-01-GU-WDQ2C1 SECTION.                                                
016820                                                                          
016830     STRING 'WDQ2C1  (WDQ2C1KY =' W-WDQ2C1KY-X ')'                        
016840          DELIMITED BY SIZE INTO SSA1                                     
016850     MOVE '  GE'               TO GODK-STATUSKODER                        
016860     CALL CBLTDLI USING GU WDQ2C-PCB DLI-IO-WDQ2C1 SSA1                   
016870     MOVE WDQ2C-STATUS-CODE    TO STATUS-WS                               
016880     PERFORM IMS-STATUSKONTROLL                                           
016890     .                                                                    
016891     EJECT                                                                
016900 IMS-STATUSKONTROLL SECTION.                                              
017000                                                                          
017100     SET STATUS-IX TO 1                                                   
017200     SEARCH GODK-STATUS                                                   
017300       AT END                                                             
017400         STRING 'FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                 
017500           DELIMITED BY SIZE INTO FELTEXT                                 
017600         DISPLAY FELTEXT                                                  
017700         CALL FELLOG                                                      
017800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
017900         CONTINUE                                                         
018000     END-SEARCH                                                           
018100     .                                                                    
