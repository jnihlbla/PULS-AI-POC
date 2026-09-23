000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W4612E00.                                                 
000400*AUTHOR.        THOMAS LARSSON.                                           
000500*DATE-WRITTEN.  DECEMBER 1996.                                            
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*                                                                         
001100*        IMPORTÖRSBEROENDE BEHANDLING (USA)                               
001200*        FILER INNEHÅLLER TRANSAR FÖR BÅDE DISTRIKT                       
001300*        7574, 8741, 8742 OCH 8743.                                       
001400*        ALLA POSTER IN OCH UT HAR KORTFORMAT                             
001500*        SKAPAR START OCH SLUTKORT                                        
001600*                                                                         
001700*        PT RIF SKALL ENDAST SISTA "RIF KORTET"                           
001800*        SKRIVAS I EN OBRUTEN SERIE  AV DYLIKA KORT                       
001900*        (ENDAST ENGELSK TEXT).                                           
002000*                                                                         
002010*                                                                         
002100     EJECT                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*- - - - - - - - - - - - INFIL:                                           
002900*                        - -  FIL TILL VIPS                               
003000     SELECT W4612E                       ASSIGN TO UT-S-W4612ED1.         
003100     SKIP2                                                                
003200*- - - - - - - - - - - - UTFIL:                                           
003300*                        - -  FIL TILL VIPS                               
003400     SELECT W4612P                       ASSIGN TO UT-S-W4612ED2.         
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 FD  W4612E                                                               
004100     RECORDING      V                                                     
004200     BLOCK CONTAINS 0.                                                    
004300     SKIP2                                                                
004400 01  INPOST                   PIC X(211).                                 
004500     SKIP2                                                                
004600 FD  W4612P                                                               
004700     RECORDING       V                                                    
004800     BLOCK CONTAINS 0.                                                    
004900 01  UTPOST                       PIC X(80).                              
005000     SKIP2                                                                
005100 01  RID-POST      -COPY W461RIDN     -L.                                 
005200     SKIP2                                                                
005300 01  RIE-POST      -COPY W461RIEN     -L.                                 
005400     SKIP2                                                                
005500 01  RIH-POST      -COPY W461RIHN     -L.                                 
005600     SKIP2                                                                
005700 01  RIO-POST      -COPY W461RIO2     -L.                                 
005710     SKIP2                                                                
005720 01  RKB-POST      -COPY W461RKBN     -L.                                 
005730     SKIP2                                                                
005740 01  RKC-POST      -COPY W461RKCN     -L.                                 
005750     SKIP2                                                                
005760 01  RKD-POST      -COPY W461RKDN     -L.                                 
005900     EJECT                                                                
006000 WORKING-STORAGE SECTION.                                                 
006001                                                                          
006010*    -- CHECKED BY WY2000                                                 
006100*- - - - - - - - - - - - - -  PROGRAM-NAMN                                
006200 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W4612E00'.            
006300     SKIP2                                                                
006400*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
006500                                                                          
006600 77  JA                          PIC X(1)    VALUE 'J'.                   
006700 77  NEJ                         PIC X(1)    VALUE 'N'.                   
006800     SKIP2                                                                
006900*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
007000                                                                          
007100 77  W4612E-EOF                  PIC X(1)    VALUE 'N'.                   
007200     SKIP2                                                                
007300*- - - - - - - - - - - - - -  ANDRA SWITCHAR                              
007400                                                                          
007500 77  W-ANT-POSTER                PIC S9(5)   COMP-3 VALUE +0.             
007600 77  W-MAX-ANT-POSTER            PIC S9(5)   COMP-3 VALUE +9000.          
007700                                                                          
007800*- - - - - - - - - - - - - -                                              
007900                                                                          
008000     EJECT                                                                
008010*      --- VALID IDDC CODES                                               
008020*                                                                         
008030*01    -COPY WWDCKONS                                                     
008040       EJECT                                                              
008100 01  DAGENS-DATUM.                                                        
008200   03  DAGENS-DATUM-AR           PIC 9(2).                                
008300   03  DAGENS-DATUM-MANAD        PIC 9(2).                                
008400   03  DAGENS-DATUM-DAG          PIC 9(2).                                
008500                                                                          
008600 01  DAGENS-TID.                                                          
008700   03  DAGENS-TID-TIM            PIC 9(2).                                
008800   03  DAGENS-TID-MIN            PIC 9(2).                                
008900   03  DAGENS-TID-SEK            PIC 9(2).                                
009000                                                                          
009100 01  DYNAMISKA-SUBPROGRAM.                                                
009200   03  ABEND                     PIC X(8)    VALUE 'ABEND'.               
009300   03  DATKORT                   PIC X(8)    VALUE 'DATKORT'.             
009400   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
009500     SKIP3                                                                
009600*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
009700                                                                          
009800 01  RETURKODER.                                                          
009900   03  RKOD                      PIC S9(4)  COMP SYNC VALUE ZERO.         
010000   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
010100   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
010200     EJECT                                                                
010300*- - - - - - - - - - - - - -  ARBETSAREAOR                                
010400                                                                          
010500******************************************************************        
010600                                                                          
010700 01  W-ARBAREA.                                                           
010800 03  W-ARBAREA-X                 PIC X(211).                              
010900     SKIP2                                                                
011000*                                                                         
012100*03  FILLER  -COPY W461RIFN        -PRE W- -RED W-ARBAREA-X               
012200     EJECT                                                                
025400                                                                          
025500*01  -COPY W461RIFN        -PRE JFR-.                                     
025600     EJECT                                                                
025700*                             STARTKORT                                   
025800*01  -COPY W461RI0N                                                       
025900     EJECT                                                                
026000*                             SLUTKORT                                    
026100*01  -COPY W461RI9                                                        
026200     EJECT                                                                
026300*- - - - - - - - - - - - - -  PARAMETRAR TILL DATKORT                     
026400                                                                          
026500 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
026600     SKIP2                                                                
026700*01  -COPY WDATKORT                                                       
026800     EJECT                                                                
026900*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
027000                                                                          
027100*01  -COPY W0005       -PRE  POSTSUM-.                                    
027200     EJECT                                                                
027300 PROCEDURE DIVISION.                                                      
027400     SKIP2                                                                
027500     PERFORM A-INIT                                                       
027600     PERFORM B-BEHANDLA                                                   
027700     PERFORM Z-FINIT                                                      
027800     MOVE ZERO TO RETURN-CODE                                             
027900     GOBACK                                                               
028000     .                                                                    
028100                                                                          
028200     SKIP3                                                                
028300 A-INIT SECTION.                                                          
028400     SKIP2                                                                
028500     OPEN INPUT W4612E OUTPUT W4612P                                      
028600     SKIP2                                                                
028700     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
028800     SKIP2                                                                
028900*    DATUM-INFORMATION HÄMTAS FRÅN DATUMKORTET                            
029000     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
029100     MOVE D-AAR TO DAGENS-DATUM-AR                                        
029200     MOVE D-MAANAD TO DAGENS-DATUM-MANAD                                  
029300     MOVE D-DAG TO DAGENS-DATUM-DAG                                       
029400     SKIP2                                                                
029500*- - - - - - - - - - - - - - - - TID                                      
029600*                                                                         
029700     ACCEPT   DAGENS-TID FROM TIME                                        
029800     .                                                                    
029900     EJECT                                                                
030000                                                                          
030100 B-BEHANDLA SECTION.                                                      
030200     SKIP2                                                                
030300     PERFORM BA-START-KORT                                                
030400     PERFORM S01-LAS-W4612E                                               
030500     PERFORM UNTIL                                                        
030600      NOT ( W4612E-EOF = NEJ )                                            
030700       IF W-RIF-IDPTYP  = 'RIF'                                           
030800         MOVE W-ARBAREA TO JFR-RIF-W461RIFN-CTX                           
030900         PERFORM S01-LAS-W4612E                                           
031000       ELSE                                                               
031100         IF JFR-RIF-IDPTYP   = 'RIF'                                      
031300           ADD     +1           TO W-ANT-POSTER                           
031400           WRITE   UTPOST   FROM JFR-RIF-W461RIFN-CTX                     
031500           MOVE SPACE TO JFR-RIF-W461RIFN-CTX                             
031600         END-IF                                                           
031700         IF W-RIF-IDPTYP = 'RIC' OR 'RIQ' OR 'RKF' OR 'RKO'               
031800           PERFORM S01-LAS-W4612E                                         
031900         ELSE                                                             
032000           EVALUATE    W-RIF-IDPTYP                                       
032100                                                                          
032200             WHEN  'RID'                                                  
032300                  WRITE RID-POST FROM INPOST                              
032400                                                                          
032500             WHEN  'RIE'                                                  
032600                  WRITE RIE-POST FROM INPOST                              
032700                                                                          
032800             WHEN  'RIH'                                                  
032900                  WRITE RIH-POST FROM INPOST                              
033000                                                                          
033010             WHEN  'RIO'                                                  
033020                  WRITE RIO-POST FROM INPOST                              
033021                                                                          
033022             WHEN  'RKB'                                                  
033023                  WRITE RKB-POST  FROM INPOST                             
033024                                                                          
033025             WHEN  'RKC'                                                  
033026                  WRITE RKC-POST  FROM INPOST                             
033027                                                                          
033028             WHEN  'RKD'                                                  
033029                  WRITE RKD-POST  FROM INPOST                             
033040                                                                          
033100             WHEN  OTHER                                                  
033200                  WRITE UTPOST   FROM INPOST                              
042300                                                                          
042400           END-EVALUATE                                                   
042500           ADD     +1           TO W-ANT-POSTER                           
042700           PERFORM S01-LAS-W4612E                                         
042800         END-IF                                                           
042900       END-IF                                                             
043000     END-PERFORM                                                          
043100     PERFORM BB-SLUT-KORT                                                 
043200     .                                                                    
043300     EJECT                                                                
090600                                                                          
090700 BA-START-KORT SECTION.                                                   
090800     SKIP2                                                                
090900     MOVE     'RI0'          TO START-IDPTYP                              
091000     MOVE     7574           TO START-IDDISTR                             
091100     MOVE     WC-CDC-SE      TO START-IDDC                                
091200     MOVE     DAGENS-DATUM   TO START-TIFILDAT                            
091300*                                                                         
091400     MOVE     DAGENS-TID     TO START-TIHHMMSS                            
091500*                                                                         
091600     DISPLAY  'START-KORT  ' START-W461RI0N-CTX                           
091700     WRITE    UTPOST         FROM  START-W461RI0N-CTX                     
091800     .                                                                    
091900     SKIP2                                                                
092000 BB-SLUT-KORT SECTION.                                                    
092100     SKIP2                                                                
092200     MOVE     'RI9'          TO SLUT-IDPTYP                               
092300     MOVE     W-ANT-POSTER   TO SLUT-KVTRANS                              
092400*                                                                         
092500     WRITE    UTPOST         FROM  SLUT-W461RI9                           
092600     DISPLAY  'SLUT-KORT   ' SLUT-W461RI9                                 
092700     .                                                                    
092800     SKIP2                                                                
092900 S01-LAS-W4612E SECTION.                                                  
093000     SKIP2                                                                
093100     READ   W4612E INTO W-ARBAREA                                         
093200     AT END MOVE JA TO W4612E-EOF                                         
093300     END-READ                                                             
093400                                                                          
093500     IF W4612E-EOF = NEJ                                                  
093600                                                                          
093700       MOVE 'W4612E'            TO POSTSUM-FDNAMN                         
093800       MOVE 'W4612ED1'          TO POSTSUM-DDNAMN2                        
093900       MOVE SPACE               TO POSTSUM-TRANSTYP                       
094000       CALL POSTSUM             USING POSTSUM-PARM                        
094100                                                                          
094200     END-IF                                                               
094300     .                                                                    
094400     EJECT                                                                
094500 Z-FINIT SECTION.                                                         
094600     SKIP2                                                                
094700                                                                          
094800     CLOSE W4612E W4612P                                                  
094900     SKIP2                                                                
095000*- - - - - - - - - - - - - - - - - - SKRIV UT ANTAL LÄSTA OCH             
095100*                                    SKRIVNA POSTER                       
095200                                                                          
095300     MOVE 'S' TO POSTSUM-OPKOD                                            
095400     CALL POSTSUM USING POSTSUM-PARM                                      
095500     .                                                                    
