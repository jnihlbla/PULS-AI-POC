000101 PROCESS DYNAM                                                            
000201*        - THE OPTION ABOVE IS NEEDED TO LINK A BMP-DB2-PGM               
000301*                                                                         
000401 ID DIVISION.                                                             
000501 PROGRAM-ID.     WF200200.                                                
000601 AUTHOR.         BO HAMMARIN.                                             
000701 DATE-WRITTEN.   MAR 2002.                                                
000801 DATE-COMPILED.                                                           
000901                                                                          
001001*   PGM                                                                   
001101*   1 CREATES DOCUMENT HEADERS CONTAINING                                 
001201*     . DOCUMENT DATE / DOCUMENT NUMBER                                   
001301*     . SENDING SYSTEM / RECEIVING SYSTEM                                 
001401*                                                                         
001501*   PGM INSERTS                                                           
001601*   - ROWS IN TABLE T01DHEA                                               
001701*                                                                         
001801*   PGM UPDATES                                                           
002001*   - ROWS IN TABLE T01NSDO (NEXT AVAILABLE DOCUMENT NUMBER)              
002101*                                                                         
002201*   PGM READS                                                             
002301*   - ROWS IN TABLE T01PROC                                               
002401*   - ROWS IN TABLE T01SLIN                                               
002501*   - ROWS IN TABLE T01FCUS                                               
002502*   - ROWS IN TABLE T01ASNS                                               
002701                                                                          
002801 ENVIRONMENT DIVISION.                                                    
002901                                                                          
003001 INPUT-OUTPUT SECTION.                                                    
003101                                                                          
003201 FILE-CONTROL.                                                            
003301                                                                          
003401 DATA DIVISION.                                                           
003501                                                                          
003601 FILE SECTION.                                                            
003701                                                                          
003801 WORKING-STORAGE SECTION.                                                 
003901*    -- CHECKED BY WY2000                                                 
004001 77  IDPGM                        PIC X(8)  VALUE 'WF200200'.             
004101 77  WS-IDSYSTEM                  PIC X(4)  VALUE 'WF02'.                 
004201                                                                          
004301 01  WS-CURRENT.                                                          
004401     03  WS-CURRENT-YEAR          PIC 9(2).                               
004501     03  WS-CURRENT-YEAR-PACKED   PIC S9(3) COMP-3.                       
004601     03  WS-DEFAULT-COUNTRY       PIC X(2)  VALUE 'XX'.                   
004701                                                                          
004801 01  WS-DIVERSE-MULTIFETCH.                                               
004901     03 WS-MX                    PIC S9(3)  COMP-3.                       
005001     03 WS-MULTIFETCH            PIC S9(3)  COMP-3.                       
005101                                                                          
005201     03 WS-SLIN-IDLEGSEL      OCCURS 100 PIC X(4).                        
005301     03 WS-SLIN-DAEXDAT       OCCURS 100 PIC X(8).                        
005401     03 WS-SLIN-TIEXTID       OCCURS 100 PIC S9(7) COMP-3.                
005501     03 WS-SLIN-KDVALISO      OCCURS 100 PIC X(3).                        
005601     03 WS-SLIN-IDLANDX3-SEND OCCURS 100 PIC X(3).                        
005701     03 WS-SLIN-IDLEVNR       OCCURS 100 PIC X(5).                        
005801     03 WS-SLIN-IDPARTNR      OCCURS 100 PIC X(9).                        
005901     03 WS-SLIN-KDFINDOC      OCCURS 100 PIC X(4).                        
006001     03 WS-SLIN-FLSOFT        OCCURS 100 PIC X(1).                        
006101     03 WS-SLIN-FLFREE        OCCURS 100 PIC X(1).                        
006201     03 WS-SLIN-FLPRIV        OCCURS 100 PIC X(1).                        
006301     03 WS-SLIN-IDBREAK-1     OCCURS 100 PIC X(8).                        
006401     03 WS-SLIN-IDBREAK-2     OCCURS 100 PIC X(8).                        
006501     03 WS-SLIN-IDSYSTEM-SEND OCCURS 100 PIC X(4).                        
006601     03 WS-SLIN-IDSYSTEM-REC  OCCURS 100 PIC X(4).                        
006701     03 WS-SLIN-BEANST        OCCURS 100 PIC X(25).                       
006801     03 WS-SLIN-IDUSER        OCCURS 100 PIC X(8).                        
006901     03 WS-SLIN-BETEXT        OCCURS 100 PIC X(125).                      
007001     03 WS-SLIN-BETEXT-CRE    OCCURS 100 PIC X(100).                      
007002     03 WS-SLIN-IDDC          OCCURS 100 PIC X(2).                        
007101     03 WS-IDFINDOC-NEXT2     OCCURS 100 PIC S9(9) COMP-3.                
007302     EJECT                                                                
007402                                                                          
007502 01  WS-ONE-LINE.                                                         
007602     03 WS-IDLEGSEL                 PIC X(4).                             
007702     03 WS-DAEXDAT                  PIC X(8).                             
007802     03 WS-TIEXTID                  PIC S9(7) COMP-3.                     
007902     03 WS-KDVALISO                 PIC X(3).                             
008002     03 WS-IDLANDX3-SEND            PIC X(3).                             
008102     03 WS-IDLEVNR                  PIC X(5).                             
008202     03 WS-IDPARTNR                 PIC X(9).                             
008302     03 WS-KDFINDOC                 PIC X(4).                             
008402     03 WS-FLSOFT                   PIC X(1).                             
008502     03 WS-FLFREE                   PIC X(1).                             
008602     03 WS-FLPRIV                   PIC X(1).                             
008702     03 WS-IDBREAK-1                PIC X(8).                             
008802     03 WS-IDBREAK-2                PIC X(8).                             
008902     03 WS-IDSYSTEM-SEND            PIC X(4).                             
009002     03 WS-IDSYSTEM-REC             PIC X(4).                             
009102     03 WS-BEANST                   PIC X(25).                            
009202     03 WS-IDUSER                   PIC X(8).                             
009302     03 WS-BETEXT                   PIC X(125).                           
009402     03 WS-BETEXT-CRE               PIC X(100).                           
009502     03 WS-IDFINDOC-NEXT            PIC S9(9) COMP-3.                     
009602     EJECT                                                                
009702     03 WS-IDSYSTEM-SEND-CHECK      PIC X(4).                             
009802                                                                          
009902 01  DYNAMISKA-SUBPROGRAM.                                                
010002*                                                                         
010102     03  ABEND                    PIC X(8)  VALUE 'ABEND   '.             
010202                                                                          
010302 01  FELTEXT                      PIC X(80) VALUE SPACE.                  
010402 01  RKOD-ABEND-DB2               PIC S9(4) VALUE +998  COMP SYNC.        
010502 01  RKOD-ABEND-MED-DUMP          PIC S9(4) VALUE +1000 COMP SYNC.        
010602     EJECT                                                                
010702*                                                                         
010703*01  -COPY WWLANDX2                                                       
010704*                                                                         
010802*        WORK-AREAS FOR DB2-SECTIONS                                      
010902*                                                                         
011002 01  FILLER                       PIC X(16)  VALUE 'PROC-TAB   '.         
011102*01  -COPY T01PROC    -PRE PROC-                                          
011202                                                                          
011302 01  FILLER                       PIC X(16)  VALUE 'SLIN-TAB   '.         
011402*01  -COPY T01SLIN    -PRE SLIN-                                          
011502                                                                          
011602 01  FILLER                       PIC X(16)  VALUE 'FCUS-TAB   '.         
011702*01  -COPY T01FCUS    -PRE CUST-                                          
011802                                                                          
012502 01  FILLER                       PIC X(16)  VALUE 'ASNS-TAB   '.         
012602*01  -COPY T01ASNS    -PRE ASNS-                                          
012702                                                                          
012802 01  FILLER                       PIC X(16)  VALUE 'NSDO-TAB   '.         
012902*01  -COPY T01NSDO    -PRE NSDO-                                          
013002                                                                          
013102 01  FILLER                       PIC X(16)  VALUE 'DHEA-TAB   '.         
013202*01  -COPY T01DHEA    -PRE DHEA-                                          
013302     EJECT                                                                
013402                                                                          
013502 01  FILLER                       PIC X(16)  VALUE 'PROC-AREA'.           
013602       EXEC SQL INCLUDE T01PROC  END-EXEC.                                
013702                                                                          
013802 01  FILLER                       PIC X(16)  VALUE 'SLIN-AREA'.           
013902       EXEC SQL INCLUDE T01SLIN  END-EXEC.                                
014002                                                                          
014102 01  FILLER                       PIC X(16)  VALUE 'CUST-AREA'.           
014202       EXEC SQL INCLUDE T01FCUS  END-EXEC.                                
014302                                                                          
015002 01  FILLER                       PIC X(16)  VALUE 'ASNS-AREA'.           
015102       EXEC SQL INCLUDE T01ASNS  END-EXEC.                                
015202                                                                          
015302 01  FILLER                       PIC X(16)  VALUE 'NSDO-AREA'.           
015402       EXEC SQL INCLUDE T01NSDO  END-EXEC.                                
015502                                                                          
015602 01  FILLER                       PIC X(16)  VALUE 'DHEA-AREA'.           
015702       EXEC SQL INCLUDE T01DHEA  END-EXEC.                                
015802     EJECT                                                                
015902                                                                          
016002 01  FILLER                       PIC X(16)  VALUE 'SQLCA-AREA'.          
016102       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
016202*                        **** STATUS-CODE FROM DB2                        
016302                                                                          
016402 01  FILLER                       PIC X(16)  VALUE 'SQLCODE-WS'.          
016502 01  DB2-WS.                                                              
016602   03  SQLCODE-WS                 PIC S9(3)  VALUE ZERO.                  
016702     88  ROW-FOUND                           VALUE +000.                  
016802     88  ROW-MISSING                         VALUE +100.                  
016902     88  ROW-OVERFLOW                        VALUE -413.                  
017002   03  GOOD-SQLCODES.                                                     
017102     05  GOOD-SQLCODE OCCURS 5                                            
017202         INDEXED BY SQLCODE-IX    PIC 999.                                
017302     EJECT                                                                
017402                                                                          
017502 PROCEDURE DIVISION.                                                      
017602 MAIN SECTION.                                                            
017702     PERFORM A-INIT                                                       
017802                                                                          
017902     PERFORM B-EXECUTE                                                    
018002                                                                          
018102     PERFORM Z-FINISH                                                     
018202     MOVE ZERO TO RETURN-CODE                                             
018302     GOBACK                                                               
018402     .                                                                    
018502     EJECT                                                                
018602                                                                          
018702 A-INIT SECTION.                                                          
018802     .                                                                    
018902     EJECT                                                                
019002                                                                          
019102 B-EXECUTE SECTION.                                                       
019202     PERFORM DB2-OPEN-CRS-SLIN                                            
019302     PERFORM DB2-FETCH-CRS-SLIN                                           
019402     IF SQLERRD(3) > 0                                                    
019502       MOVE 000     TO SQLCODE-WS                                         
019602     END-IF                                                               
019702     PERFORM UNTIL ROW-MISSING                                            
019802       MOVE SQLERRD(3) TO WS-MULTIFETCH                                   
019902       MOVE ZERO       TO WS-MX                                           
020002       PERFORM UNTIL WS-MX = WS-MULTIFETCH                                
020102         ADD +1        TO WS-MX                                           
020202         MOVE WS-SLIN-IDLEGSEL(WS-MX)      TO WS-IDLEGSEL                 
020302         MOVE WS-SLIN-DAEXDAT(WS-MX)       TO WS-DAEXDAT                  
020402         MOVE WS-SLIN-TIEXTID(WS-MX)       TO WS-TIEXTID                  
020502         MOVE WS-SLIN-KDVALISO(WS-MX)      TO WS-KDVALISO                 
020702         MOVE WS-SLIN-IDLEVNR(WS-MX)       TO WS-IDLEVNR                  
020802         MOVE WS-SLIN-IDPARTNR(WS-MX)      TO WS-IDPARTNR                 
020902         MOVE WS-SLIN-KDFINDOC(WS-MX)      TO WS-KDFINDOC                 
021002         MOVE WS-SLIN-FLSOFT(WS-MX)        TO WS-FLSOFT                   
021102         MOVE WS-SLIN-FLFREE(WS-MX)        TO WS-FLFREE                   
021202         MOVE WS-SLIN-FLPRIV(WS-MX)        TO WS-FLPRIV                   
021302         MOVE WS-SLIN-IDBREAK-1(WS-MX)     TO WS-IDBREAK-1                
021402         MOVE WS-SLIN-IDBREAK-2(WS-MX)     TO WS-IDBREAK-2                
021502         MOVE SPACE                        TO WS-IDSYSTEM-SEND            
021602         MOVE SPACE                        TO WS-IDSYSTEM-REC             
021702         MOVE SPACE                        TO WS-BEANST                   
021802         MOVE SPACE                        TO WS-IDUSER                   
021902         MOVE SPACE                        TO WS-BETEXT                   
022002         MOVE SPACE                        TO WS-BETEXT-CRE               
022102         MOVE WS-SLIN-IDSYSTEM-SEND(WS-MX) TO                             
022202                     WS-IDSYSTEM-SEND-CHECK                               
022203**** HERE WE CHECK IF THERE IS A DDGS FROM EU                             
022204         MOVE WS-SLIN-IDDC(WS-MX) TO LANDX2-IDLANDX2                      
022205         IF LANDX2-EU-IDLANDX2                                            
022206           MOVE WS-SLIN-IDLANDX3-SEND(WS-MX) TO WS-IDLANDX3-SEND          
022207*          IF WS-SLIN-IDPARTNR(WS-MX) = '17754'                           
022208*            MOVE 'IT'                       TO WS-IDLANDX3-SEND          
022209*          END-IF                                                         
022210           IF WS-SLIN-IDPARTNR(WS-MX) = '119613'                          
022211             MOVE 'PL'                       TO WS-IDLANDX3-SEND          
022212           END-IF                                                         
022213         ELSE                                                             
022220           MOVE WS-SLIN-IDLANDX3-SEND(WS-MX) TO WS-IDLANDX3-SEND          
022902         END-IF                                                           
022903                                                                          
027305         PERFORM DB2-SELECT-NSDO-VERS1                                    
027405         IF ROW-MISSING                                                   
027505* DIRTY FIX FOR USING DEFAULT DOCNO-SERIE                                 
027605* WHEN ITALIAN VCCS-RETURNS TO SDC25                                      
027705         OR (WS-IDLEGSEL          = 'VCCS'                                
027805         AND WS-IDLANDX3-SEND = 'IT'                                      
027905         AND WS-IDSYSTEM-SEND-CHECK = 'W41X')                             
028005         OR (WS-IDLEGSEL          = 'VCCS'                                
028105         AND WS-IDLANDX3-SEND = 'IT'                                      
028205         AND WS-IDSYSTEM-SEND-CHECK = 'W47X')                             
028206         MOVE WS-SLIN-IDDC(WS-MX) TO LANDX2-IDLANDX2                      
028207           IF LANDX2-EU-IDLANDX2                                          
028209*            IF (WS-SLIN-IDPARTNR(WS-MX) = '17754'                        
028230*            OR WS-SLIN-IDPARTNR(WS-MX) = '119613')                       
028231             IF WS-SLIN-IDPARTNR(WS-MX) = '119613'                        
028232               PERFORM DB2-SELECT-NSDO-VERS3                              
028240             ELSE                                                         
028605               PERFORM DB2-SELECT-NSDO-VERS2                              
028606             END-IF                                                       
028607           ELSE                                                           
028608             PERFORM DB2-SELECT-NSDO-VERS2                                
028609           END-IF                                                         
028705* UPDATE NEXT AVAILABLE DOC.NUMBER WHEN NO COUNTRY CODE                   
028805           PERFORM DB2-UPDATE-NSDO-VERS1                                  
028905           IF WS-IDFINDOC-NEXT > NSDO-IDFINDOC-STOP                       
029005             PERFORM DB2-UPDATE-NSDO-VERS2                                
029105           END-IF                                                         
029205           IF WS-IDFINDOC-NEXT = NSDO-IDFINDOC-STOP                       
029305             PERFORM DB2-UPDATE-NSDO-VERS2                                
029405           END-IF                                                         
029505         ELSE                                                             
029605         MOVE 'BBBB'              TO  WS-IDSYSTEM-SEND                    
029705* UPDATE NEXT AVAILABLE DOC.NUMBER WHEN COUNTRY CODE                      
029805           PERFORM DB2-UPDATE-NSDO-VERS1                                  
029905           IF WS-IDFINDOC-NEXT > NSDO-IDFINDOC-STOP                       
030005             PERFORM DB2-UPDATE-NSDO-VERS2                                
030105           END-IF                                                         
030205           IF WS-IDFINDOC-NEXT = NSDO-IDFINDOC-STOP                       
030305             PERFORM DB2-UPDATE-NSDO-VERS2                                
030405           END-IF                                                         
030505         END-IF                                                           
030609         MOVE WS-IDFINDOC-NEXT TO WS-IDFINDOC-NEXT2(WS-MX)                
030905       END-PERFORM                                                        
031005       PERFORM DB2-INSERT-DHEA                                            
031105       IF WS-MULTIFETCH = 100                                             
031205         PERFORM DB2-FETCH-CRS-SLIN                                       
031305         IF SQLERRD(3) > 0                                                
031405           MOVE 000     TO SQLCODE-WS                                     
031505         END-IF                                                           
031605       ELSE                                                               
031705         MOVE 100 TO SQLCODE-WS                                           
031805       END-IF                                                             
031905     END-PERFORM                                                          
032005     .                                                                    
032105     EJECT                                                                
032205                                                                          
032305 Z-FINISH SECTION.                                                        
032405     PERFORM DB2-CLOSE-CRS-SLIN                                           
032505     .                                                                    
032605     EJECT                                                                
032705                                                                          
032805* --- DB2 SECTIONS  ---                                                   
032905*                                                                         
033005                                                                          
033105 DB2-OPEN-CRS-SLIN SECTION.                                               
033205     EXEC SQL                                                             
033305              DECLARE SLIN-CRS CURSOR WITH ROWSET POSITIONING FOR         
033405              SELECT DISTINCT                                             
033505              T01SLIN.IDLEGSEL,                                           
033605              T01SLIN.DAEXDAT,                                            
033705              T01SLIN.TIEXTID,                                            
033805              T01SLIN.KDVALISO,                                           
033905              T01SLIN.IDLANDX3_SEND,                                      
034005              T01SLIN.IDLEVNR,                                            
034105              T01SLIN.IDPARTNR,                                           
034205              T01SLIN.KDFINDOC,                                           
034305              T01SLIN.FLSOFT,                                             
034405              T01SLIN.FLFREE,                                             
034505              T01SLIN.FLPRIV,                                             
034605              T01SLIN.IDBREAK_1,                                          
034705              T01SLIN.IDBREAK_2,                                          
034706              T01SLIN.IDDC,                                               
034805              T01SLIN.IDSYSTEM_SEND                                       
034905                                                                          
035005     FROM     T01PROC,                                                    
035105              T01SLIN                                                     
035205                                                                          
035305     WHERE    T01PROC.IDSYSTEM = 'WF02'                                   
035405          AND T01SLIN.IDLEGSEL = T01PROC.IDLEGSEL                         
035505          AND T01SLIN.DAEXDAT  = T01PROC.DAEXDAT                          
035605          AND T01SLIN.TIEXTID  = T01PROC.TIEXTID                          
035705     END-EXEC                                                             
035805                                                                          
035905     EXEC SQL OPEN SLIN-CRS                                               
036005     END-EXEC                                                             
036105                                                                          
036205     MOVE 000            TO GOOD-SQLCODES                                 
036305     MOVE SQLCODE        TO SQLCODE-WS                                    
036405     PERFORM DB2-STATUS-CHECK                                             
036505     .                                                                    
036605     EJECT                                                                
036705                                                                          
036805 DB2-FETCH-CRS-SLIN SECTION.                                              
036905     EXEC SQL                                                             
037005            FETCH NEXT ROWSET FROM SLIN-CRS FOR 100 ROWS                  
037105       INTO :WS-SLIN-IDLEGSEL,                                            
037205            :WS-SLIN-DAEXDAT,                                             
037305            :WS-SLIN-TIEXTID,                                             
037405            :WS-SLIN-KDVALISO,                                            
037505            :WS-SLIN-IDLANDX3-SEND,                                       
037605            :WS-SLIN-IDLEVNR,                                             
037705            :WS-SLIN-IDPARTNR,                                            
037805            :WS-SLIN-KDFINDOC,                                            
037905            :WS-SLIN-FLSOFT,                                              
038005            :WS-SLIN-FLFREE,                                              
038105            :WS-SLIN-FLPRIV,                                              
038205            :WS-SLIN-IDBREAK-1,                                           
038305            :WS-SLIN-IDBREAK-2,                                           
038306            :WS-SLIN-IDDC,                                                
038405            :WS-SLIN-IDSYSTEM-SEND                                        
038505     END-EXEC                                                             
038605                                                                          
038705     MOVE 000100         TO GOOD-SQLCODES                                 
038805     MOVE SQLCODE        TO SQLCODE-WS                                    
038905     PERFORM DB2-STATUS-CHECK                                             
039005     .                                                                    
039105     EJECT                                                                
039205                                                                          
053105 DB2-SELECT-NSDO-VERS1 SECTION.                                           
053205     EXEC SQL                                                             
053305     SELECT   DISTINCT                                                    
053405              T01NSDO.IDLEGSEL,                                           
053505              T01NSDO.IDLOPNR,                                            
053605              T01NSDO.IDFINDOC_NEXT,                                      
053705              T01NSDO.IDFINDOC_STOP                                       
053805                                                                          
053905     INTO    :NSDO-IDLEGSEL,                                              
054005             :NSDO-IDLOPNR,                                               
054105             :WS-IDFINDOC-NEXT,                                           
054205             :NSDO-IDFINDOC-STOP                                          
054305                                                                          
054405     FROM     T01NSDO,                                                    
054505              T01SLIN,                                                    
054605              T01FCUS,                                                    
054705              T01ASNS                                                     
054805                                                                          
054905     WHERE    T01SLIN.IDLEGSEL      = :WS-IDLEGSEL                        
055005     AND      T01SLIN.DAEXDAT       = :WS-DAEXDAT                         
055105     AND      T01SLIN.TIEXTID       = :WS-TIEXTID                         
055205     AND      T01SLIN.KDVALISO      = :WS-KDVALISO                        
055305     AND      T01SLIN.IDLANDX3_SEND = :WS-IDLANDX3-SEND                   
055405     AND      T01SLIN.IDLEVNR       = :WS-IDLEVNR                         
055505     AND      T01SLIN.IDPARTNR      = :WS-IDPARTNR                        
055605     AND      T01SLIN.KDFINDOC      = :WS-KDFINDOC                        
055705     AND      T01SLIN.FLSOFT        = :WS-FLSOFT                          
055805     AND      T01SLIN.FLFREE        = :WS-FLFREE                          
055905     AND      T01SLIN.FLPRIV        = :WS-FLPRIV                          
056005     AND      T01SLIN.IDBREAK_1     = :WS-IDBREAK-1                       
056105     AND      T01SLIN.IDBREAK_2     = :WS-IDBREAK-2                       
056205     AND      T01FCUS.IDLEGSEL      = T01SLIN.IDLEGSEL                    
056305     AND      T01FCUS.IDPARTNR      = T01SLIN.IDPARTNR                    
056405     AND      T01FCUS.KDSTATUS      = 1                                   
056505     AND      T01FCUS.DADELDAT      = '00000000'                          
056605     AND      T01ASNS.IDLEGSEL      = T01FCUS.IDLEGSEL                    
056705     AND      T01ASNS.KDFINDOC      = T01SLIN.KDFINDOC                    
056805     AND      T01ASNS.KDPARTTY      = T01FCUS.KDPARTTY                    
056905     AND      T01ASNS.KDPARTGR      = T01FCUS.KDPARTGR                    
057005     AND      T01ASNS.IDLANDX3      = T01SLIN.IDLANDX3_SEND               
057105     AND      T01NSDO.IDLEGSEL      = T01ASNS.IDLEGSEL                    
057205     AND      T01NSDO.IDLOPNR       = T01ASNS.IDLOPNR                     
057305     END-EXEC                                                             
057405                                                                          
057505     MOVE 000100         TO GOOD-SQLCODES                                 
057605     MOVE SQLCODE        TO SQLCODE-WS                                    
057705     PERFORM DB2-STATUS-CHECK                                             
057805     .                                                                    
057905     EJECT                                                                
058005                                                                          
058105 DB2-SELECT-NSDO-VERS2 SECTION.                                           
058205     EXEC SQL                                                             
058305     SELECT   DISTINCT                                                    
058405              T01NSDO.IDLEGSEL,                                           
058505              T01NSDO.IDLOPNR,                                            
058605              T01NSDO.IDFINDOC_NEXT,                                      
058705              T01NSDO.IDFINDOC_STOP                                       
058805                                                                          
058905     INTO    :NSDO-IDLEGSEL,                                              
059005             :NSDO-IDLOPNR,                                               
059105             :WS-IDFINDOC-NEXT,                                           
059205             :NSDO-IDFINDOC-STOP                                          
059305                                                                          
059405     FROM     T01NSDO,                                                    
059505              T01SLIN,                                                    
059605              T01FCUS,                                                    
059705              T01ASNS                                                     
059805                                                                          
059905     WHERE    T01SLIN.IDLEGSEL      = :WS-IDLEGSEL                        
060005     AND      T01SLIN.DAEXDAT       = :WS-DAEXDAT                         
060105     AND      T01SLIN.TIEXTID       = :WS-TIEXTID                         
060205     AND      T01SLIN.KDVALISO      = :WS-KDVALISO                        
060305     AND      T01SLIN.IDLANDX3_SEND = :WS-IDLANDX3-SEND                   
060405     AND      T01SLIN.IDLEVNR       = :WS-IDLEVNR                         
060505     AND      T01SLIN.IDPARTNR      = :WS-IDPARTNR                        
060605     AND      T01SLIN.KDFINDOC      = :WS-KDFINDOC                        
060705     AND      T01SLIN.FLSOFT        = :WS-FLSOFT                          
060805     AND      T01SLIN.FLFREE        = :WS-FLFREE                          
060905     AND      T01SLIN.FLPRIV        = :WS-FLPRIV                          
061005     AND      T01SLIN.IDBREAK_1     = :WS-IDBREAK-1                       
061105     AND      T01SLIN.IDBREAK_2     = :WS-IDBREAK-2                       
061205     AND      T01FCUS.IDLEGSEL      = T01SLIN.IDLEGSEL                    
061305     AND      T01FCUS.IDPARTNR      = T01SLIN.IDPARTNR                    
061405     AND      T01FCUS.KDSTATUS      = 1                                   
061505     AND      T01FCUS.DADELDAT      = '00000000'                          
061605     AND      T01ASNS.IDLEGSEL      = T01FCUS.IDLEGSEL                    
061705     AND      T01ASNS.KDFINDOC      = T01SLIN.KDFINDOC                    
061805     AND      T01ASNS.KDPARTTY      = T01FCUS.KDPARTTY                    
061905     AND      T01ASNS.KDPARTGR      = T01FCUS.KDPARTGR                    
062005     AND      T01ASNS.IDLANDX3      = :WS-DEFAULT-COUNTRY                 
062105     AND      T01NSDO.IDLEGSEL      = T01ASNS.IDLEGSEL                    
062205     AND      T01NSDO.IDLOPNR       = T01ASNS.IDLOPNR                     
062305     END-EXEC                                                             
062405                                                                          
062505     MOVE 000            TO GOOD-SQLCODES                                 
062605     MOVE SQLCODE        TO SQLCODE-WS                                    
062705     PERFORM DB2-STATUS-CHECK                                             
062805     .                                                                    
062905     EJECT                                                                
063005                                                                          
063006 DB2-SELECT-NSDO-VERS3 SECTION.                                           
063007     EXEC SQL                                                             
063008     SELECT   DISTINCT                                                    
063009              T01NSDO.IDLEGSEL,                                           
063010              T01NSDO.IDLOPNR,                                            
063020              T01NSDO.IDFINDOC_NEXT,                                      
063030              T01NSDO.IDFINDOC_STOP                                       
063040                                                                          
063050     INTO    :NSDO-IDLEGSEL,                                              
063060             :NSDO-IDLOPNR,                                               
063070             :WS-IDFINDOC-NEXT,                                           
063080             :NSDO-IDFINDOC-STOP                                          
063090                                                                          
063100     FROM     T01NSDO,                                                    
063101              T01SLIN,                                                    
063102              T01FCUS,                                                    
063103              T01ASNS                                                     
063104                                                                          
063105     WHERE    T01SLIN.IDLEGSEL      = :WS-IDLEGSEL                        
063106     AND      T01SLIN.DAEXDAT       = :WS-DAEXDAT                         
063107     AND      T01SLIN.TIEXTID       = :WS-TIEXTID                         
063108     AND      T01SLIN.KDVALISO      = :WS-KDVALISO                        
063110     AND      T01SLIN.IDLEVNR       = :WS-IDLEVNR                         
063111     AND      T01SLIN.IDPARTNR      = :WS-IDPARTNR                        
063112     AND      T01SLIN.KDFINDOC      = :WS-KDFINDOC                        
063113     AND      T01SLIN.FLSOFT        = :WS-FLSOFT                          
063114     AND      T01SLIN.FLFREE        = :WS-FLFREE                          
063115     AND      T01SLIN.FLPRIV        = :WS-FLPRIV                          
063116     AND      T01SLIN.IDBREAK_1     = :WS-IDBREAK-1                       
063117     AND      T01SLIN.IDBREAK_2     = :WS-IDBREAK-2                       
063118     AND      T01FCUS.IDLEGSEL      = T01SLIN.IDLEGSEL                    
063119     AND      T01FCUS.IDPARTNR      = T01SLIN.IDPARTNR                    
063120     AND      T01FCUS.KDSTATUS      = 1                                   
063121     AND      T01FCUS.DADELDAT      = '00000000'                          
063122     AND      T01ASNS.IDLEGSEL      = T01FCUS.IDLEGSEL                    
063123     AND      T01ASNS.KDFINDOC      = T01SLIN.KDFINDOC                    
063124     AND      T01ASNS.KDPARTTY      = T01FCUS.KDPARTTY                    
063125     AND      T01ASNS.KDPARTGR      = T01FCUS.KDPARTGR                    
063126     AND      T01ASNS.IDLANDX3      = :WS-IDLANDX3-SEND                   
063127     AND      T01NSDO.IDLEGSEL      = T01ASNS.IDLEGSEL                    
063128     AND      T01NSDO.IDLOPNR       = T01ASNS.IDLOPNR                     
063129     END-EXEC                                                             
063130                                                                          
063131     MOVE 000100         TO GOOD-SQLCODES                                 
063132     MOVE SQLCODE        TO SQLCODE-WS                                    
063133     PERFORM DB2-STATUS-CHECK                                             
063134     .                                                                    
063135     EJECT                                                                
063136                                                                          
063140 DB2-UPDATE-NSDO-VERS1 SECTION.                                           
063205     EXEC SQL UPDATE T01NSDO                                              
063305     SET      IDFINDOC_NEXT = IDFINDOC_NEXT + 1                           
063405                                                                          
063505     WHERE    T01NSDO.IDLEGSEL      = :NSDO-IDLEGSEL                      
063605     AND      T01NSDO.IDLOPNR       = :NSDO-IDLOPNR                       
063705     END-EXEC                                                             
063805                                                                          
063905     MOVE 000            TO GOOD-SQLCODES                                 
064005     MOVE SQLCODE        TO SQLCODE-WS                                    
064105     PERFORM DB2-STATUS-CHECK                                             
064205     .                                                                    
064305     EJECT                                                                
064405                                                                          
064505 DB2-UPDATE-NSDO-VERS2 SECTION.                                           
064605     EXEC SQL UPDATE T01NSDO                                              
064705     SET      IDFINDOC_NEXT = IDFINDOC_START                              
064805                                                                          
064905     WHERE    T01NSDO.IDLEGSEL      = :NSDO-IDLEGSEL                      
065005     AND      T01NSDO.IDLOPNR       = :NSDO-IDLOPNR                       
065105     END-EXEC                                                             
065205                                                                          
065305     MOVE 000            TO GOOD-SQLCODES                                 
065405     MOVE SQLCODE        TO SQLCODE-WS                                    
065505     PERFORM DB2-STATUS-CHECK                                             
065605     .                                                                    
065705     EJECT                                                                
065805                                                                          
065905 DB2-INSERT-DHEA SECTION.                                                 
066005     EXEC SQL INSERT INTO T01DHEA                                         
066105        (                                                                 
066205         IDLEGSEL,                                                        
066305         DAEXDAT,                                                         
066405         TIEXTID,                                                         
066505         KDVALISO,                                                        
066605         IDLANDX3_SEND,                                                   
066705         IDLEVNR,                                                         
066805         IDPARTNR,                                                        
066905         KDFINDOC,                                                        
067005         FLSOFT,                                                          
067105         FLFREE,                                                          
067205         FLPRIV,                                                          
067305         IDBREAK_1,                                                       
067405         IDBREAK_2,                                                       
067505         DAFINDOC,                                                        
067605         IDFINDOC,                                                        
067705         IDSYSTEM_SEND,                                                   
067805         IDSYSTEM_REC,                                                    
067905         BEANST,                                                          
068005         IDUSER,                                                          
068105         BETEXT,                                                          
068205         BETEXT_CRE                                                       
068305        )                                                                 
068405       VALUES                                                             
068505        (                                                                 
068605         :WS-SLIN-IDLEGSEL,                                               
068705         :WS-SLIN-DAEXDAT,                                                
068805         :WS-SLIN-TIEXTID,                                                
068905         :WS-SLIN-KDVALISO,                                               
069005         :WS-SLIN-IDLANDX3-SEND,                                          
069105         :WS-SLIN-IDLEVNR,                                                
069205         :WS-SLIN-IDPARTNR,                                               
069305         :WS-SLIN-KDFINDOC,                                               
069405         :WS-SLIN-FLSOFT,                                                 
069505         :WS-SLIN-FLFREE,                                                 
069605         :WS-SLIN-FLPRIV,                                                 
069705         :WS-SLIN-IDBREAK-1,                                              
069805         :WS-SLIN-IDBREAK-2,                                              
069905         :WS-SLIN-DAEXDAT,                                                
070005         :WS-IDFINDOC-NEXT2,                                              
070206         :WS-IDSYSTEM-SEND,                                               
070306         :WS-IDSYSTEM-REC,                                                
070406         :WS-BEANST,                                                      
070506         :WS-IDUSER,                                                      
070606         :WS-BETEXT,                                                      
070706         :WS-BETEXT-CRE                                                   
070806        ) FOR :WS-MULTIFETCH ROWS ATOMIC                                  
070906     END-EXEC                                                             
071006                                                                          
071106     MOVE 000            TO GOOD-SQLCODES                                 
071206     MOVE SQLCODE        TO SQLCODE-WS                                    
071306     PERFORM DB2-STATUS-CHECK                                             
071406     .                                                                    
071506     EJECT                                                                
071606                                                                          
071706 DB2-CLOSE-CRS-SLIN SECTION.                                              
071806     EXEC SQL                                                             
071906         CLOSE SLIN-CRS                                                   
072006     END-EXEC                                                             
072106     .                                                                    
072206     EJECT                                                                
072306                                                                          
072406 DB2-STATUS-CHECK SECTION.                                                
072506     SET SQLCODE-IX         TO 1                                          
072606     SEARCH GOOD-SQLCODE AT END                                           
072706           CALL ABEND USING RKOD-ABEND-DB2                                
072806        WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                       
072906           CONTINUE                                                       
073000     END-SEARCH                                                           
080000     .                                                                    
