000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W3301900.                                                 
000400 AUTHOR.        RONNY STENHOLM.                                           
000500 DATE-WRITTEN.  NOVEMBER 1989.                                            
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        LÄSER TVÅ FILER OCH EN TABELL OCH SKAPAR FRÅN DESSA              
001000*        EN ARTIKELINFORMATIONSFIL PÅ DE ARTIKLAR SOM FINNS               
001100*        PÅ PRIMÄWREGISTRET TILL ARTIKELSTATISTIKEN.                      
001200*                                                                         
001300*    SUBPROGRAM:                                                          
001400*               WKPSKONV - HÄMTAR VIA PRODSLAGSKOD BENÄMNINGEN            
001500*               PÅ PRODUKTSLAGET.                                         
001600     EJECT                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*    --- INFILER:                                                         
002400                                                                          
002500     SELECT W33013                       ASSIGN TO W33019D1.              
002700     SELECT W33017                       ASSIGN TO W33019D3.              
002800     SELECT W01160                       ASSIGN TO W33019D4.              
002900     SELECT W33099                       ASSIGN TO W33019D5.              
003000                                                                          
003100*    --- UTFIL:                                                           
003200*           --- ARTIKELINFORMATION:                                       
003300     SELECT W33019                       ASSIGN TO W33019D6.              
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP2                                                                
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
003900 FD  W33013                                                               
004000     LABEL RECORD   STANDARD                                              
004100     RECORDING      V                                                     
004200     BLOCK CONTAINS 0.                                                    
004300     SKIP2                                                                
004400*01  -COPY W330310     -L.                                                
004600     SKIP2                                                                
004700*01  -COPY W330300     -L.                                                
004900     SKIP2                                                                
005000*01  -COPY W330320     -L.                                                
005200     SKIP2                                                                
005300*01  -COPY W330330     -L.                                                
005500     SKIP2                                                                
005600*01  -COPY W330340     -L.                                                
005800     SKIP2                                                                
005900*01  -COPY W330350     -L.                                                
006100     EJECT                                                                
007000 FD  W33017                                                               
007100     LABEL RECORD   STANDARD                                              
007200     RECORDING      F                                                     
007300     BLOCK CONTAINS 0.                                                    
007400     SKIP2                                                                
007500*01  -COPY W33017      -L.                                                
007700     EJECT                                                                
007800 FD  W01160                                                               
007900     LABEL RECORD   STANDARD                                              
008000     RECORDING      F                                                     
008100     BLOCK CONTAINS 0.                                                    
008200     SKIP2                                                                
008300*01  -COPY W01160      -L.                                                
008500     EJECT                                                                
008600 FD  W33099                                                               
008700     LABEL RECORD   STANDARD                                              
008800     RECORDING      F                                                     
008900     BLOCK CONTAINS 0.                                                    
009000     SKIP2                                                                
009100*01  -COPY W33099      -L.                                                
009300     EJECT                                                                
009400 FD  W33019                                                               
009500     LABEL RECORD   STANDARD                                              
009600     RECORDING      F                                                     
009700     BLOCK CONTAINS 0.                                                    
009800     SKIP2                                                                
009900*01  UT-POST  -COPY W33019       -L.                                      
010100     EJECT                                                                
010200 WORKING-STORAGE SECTION.                                                 
010300     SKIP2                                                                
010301                                                                          
010310*    -- CHECKED BY WY2000                                                 
010400 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W3301900'.            
010500 77  JA                          PIC X(1)    VALUE 'J'.                   
010600 77  NEJ                         PIC X(1)    VALUE 'N'.                   
010700 77  INFIL-13-EOF                PIC X(1)    VALUE 'N'.                   
010800 77  INFIL-16-EOF                PIC X(1)    VALUE 'N'.                   
010900 77  INFIL-17-EOF                PIC X(1)    VALUE 'N'.                   
011000 77  INFIL-LB-EOF                PIC X(1)    VALUE 'N'.                   
011100 77  INFIL-BEFKNGRP-EOF          PIC X(1)    VALUE 'N'.                   
011200 77  SOEK-IX-MAX                 PIC S9(4)   VALUE +1  COMP SYNC.         
011300 77  ARB-IX                      PIC S9(4)   VALUE +1  COMP SYNC.         
011400 77  SPAR-IDARTNR                PIC S9(9)   VALUE ZERO COMP-3.           
011500 77  WS-IDARTNR                  PIC S9(9)   VALUE ZERO COMP-3.           
011600                                                                          
011700     EJECT                                                                
011800 01  DYNAMISKA-SUBPROGRAM.                                                
011900*                                                                         
012000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
012100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG'.              
012200     03  WKPSKONV                PIC X(8)    VALUE 'WKPSKONV'.            
012300     SKIP2                                                                
012400*- - - - - - - - - - - - - - - - PARAMETRAR TILL POSTSUM                  
012500*                                                                         
012600 01  FILLER                       PIC X(16)  VALUE 'POSTSUM'.             
012700*01  -COPY W0005      -PRE  POSTSUM-                                      
012900     EJECT                                                                
013000*- - - - - - - - - - - - - - - - PARAMETRAR TILL WKPSKONV                 
013100*                                                                         
013200 01  FILLER                       PIC X(16)  VALUE 'WKPSKONV'.            
013300*01  -COPY WKPSAREA                                                       
013500     EJECT                                                                
013600*- - - - - - - - - - - - - - - - SOEK-AREA                                
013700*                                                                         
013800 01  FILLER                       PIC X(16)  VALUE 'SOEK-AREA'.           
013900 01  SOEK-AREA.                                                           
014000     03  SOEK-RAD OCCURS 9999.                                            
014100         05  SOEK-INDEX           PIC S9(5)  COMP-3.                      
014200         05  SOEK-BEFKNGRP        PIC X(50).                              
014300     EJECT                                                                
014400*- - - - - - - - - - - - - - - - UT-AREA                                  
014500*                                                                         
014600 01  FILLER                      PIC X(16)  VALUE 'UTAREA19'.             
014700 01  UT9-AREA                    PIC X(135).                              
014800*01  AREA  -PRE UT19- -COPY W33019      -RED UT9-AREA.                    
015000     EJECT                                                                
015100*- - - - - - - - - - - - - - - - IN-AREA                                  
015200*                                                                         
015300 01  FILLER                      PIC X(16)  VALUE 'INAREA13'.             
015400 01  IN3-AREA                    PIC X(28).                               
015500*01  AREA  -PRE IN13- -COPY W330300     -RED IN3-AREA.                    
015700     SKIP2                                                                
016300 01  FILLER                      PIC X(16)  VALUE 'INAREA17'.             
016400 01  IN7-AREA                    PIC X(55).                               
016500*01  AREA  -PRE IN17- -COPY W33017      -RED IN7-AREA.                    
016700     SKIP2                                                                
016800 01  FILLER                      PIC X(15)  VALUE 'INAREA-LB'.            
016900 01  LB-AREA                    PIC X(950).                               
017000*01  AREA  -PRE DLB- -COPY W01160       -RED LB-AREA.                     
017200     SKIP2                                                                
017300 01  FILLER                      PIC X(16)  VALUE 'INAREATAB'.            
017400 01  TAB-AREA                    PIC X(59).                               
017500*01  AREA  -PRE INTAB- -COPY W33099      -RED TAB-AREA.                   
017700     SKIP2                                                                
017800     EJECT                                                                
017900 PROCEDURE DIVISION.                                                      
018000     SKIP2                                                                
018100 STYR SECTION.                                                            
018200     PERFORM A-INIT                                                       
018300     PERFORM B-SKAPA-TABELL                                               
018400     PERFORM S01-LAS-13-FIL                                               
018600     PERFORM S17-LAS-17-FIL                                               
018700     PERFORM S18-LAS-LB-FIL                                               
018800     PERFORM UNTIL INFIL-13-EOF = JA                                      
018900       MOVE IN13-IDARTNR TO UT19-IDARTNR                                  
019100       PERFORM D-HAMTA-INFO-FRAN-17-FIL                                   
019200       PERFORM E-HAMTA-INFO-FRAN-LB-FIL                                   
019310       MOVE SOEK-BEFKNGRP(UT19-IDFKNGRP) TO                               
019400                              UT19-BEFKNGRP                               
019500       PERFORM F-HAMTA-PRODSLAG-BEN                                       
019600       PERFORM S19-SKRIV-19-FIL                                           
019700       PERFORM S01-LAS-13-FIL                                             
019800     END-PERFORM                                                          
019900     PERFORM Z-FINIT                                                      
020000     MOVE ZERO TO RETURN-CODE                                             
020100     GOBACK                                                               
020200     .                                                                    
020300     EJECT                                                                
020400 A-INIT SECTION.                                                          
020500     SKIP2                                                                
020600     OPEN INPUT W33013                                                    
020800                W33017                                                    
020900                W01160                                                    
021000                W33099                                                    
021100     OPEN OUTPUT W33019                                                   
021200                                                                          
021300     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
021400                                                                          
021500     INITIALIZE SOEK-AREA                                                 
021600     .                                                                    
021700     EJECT                                                                
021800 B-SKAPA-TABELL  SECTION.                                                 
021900                                                                          
022000     PERFORM S02-LAS-TABELL-FIL                                           
022100     PERFORM UNTIL INFIL-BEFKNGRP-EOF = JA                                
022200       IF INTAB-IDFKNGRP > +0                                             
022210         MOVE INTAB-BEFKNGRP TO SOEK-BEFKNGRP(INTAB-IDFKNGRP)             
022220       END-IF                                                             
022300       PERFORM S02-LAS-TABELL-FIL                                         
022400     END-PERFORM                                                          
022500     .                                                                    
022600                                                                          
024000 D-HAMTA-INFO-FRAN-17-FIL  SECTION.                                       
024100     PERFORM UNTIL WS-IDARTNR NOT > IN17-IDARTNR                          
024200       PERFORM S17-LAS-17-FIL                                             
024300     END-PERFORM                                                          
024400     IF WS-IDARTNR = IN17-IDARTNR                                         
024500       MOVE IN17-BEART-SVE TO UT19-BEART-SVE                              
024600       MOVE IN17-BEART-ENG TO UT19-BEART-ENG                              
024700     ELSE                                                                 
024800       MOVE SPACE          TO UT19-BEART-SVE                              
024900                              UT19-BEART-ENG                              
025000     END-IF                                                               
025100     .                                                                    
025200     EJECT                                                                
025300 E-HAMTA-INFO-FRAN-LB-FIL  SECTION.                                       
025400     PERFORM UNTIL WS-IDARTNR NOT > DLB-CLAG-IDARTNR                      
025500       PERFORM S18-LAS-LB-FIL                                             
025600     END-PERFORM                                                          
025700     IF WS-IDARTNR = DLB-CLAG-IDARTNR                                     
025800       MOVE DLB-CLAG-IDLEVNR TO UT19-IDLEVNR                              
025900       MOVE DLB-CLAG-IDANSK TO UT19-IDANSK                                
026000       MOVE DLB-CLAG-KDVVKL TO UT19-KDVVKL                                
026100       MOVE DLB-CLAG-IDLKTO TO UT19-IDLKTO                                
026110       MOVE DLB-CLAG-KDPRODSL TO UT19-KDPRODSL                            
026120       MOVE DLB-CLAG-IDFKNGRP TO UT19-IDFKNGRP                            
026200     ELSE                                                                 
026300       MOVE SPACE          TO UT19-IDLEVNR                                
026400       MOVE ZERO           TO UT19-IDANSK                                 
026500                              UT19-KDVVKL                                 
026600                              UT19-IDLKTO                                 
026610                              UT19-KDPRODSL                               
026620       MOVE +9999          TO UT19-IDFKNGRP                               
026700     END-IF                                                               
026800     .                                                                    
026900     EJECT                                                                
027000 F-HAMTA-PRODSLAG-BEN SECTION.                                            
027100                                                                          
027200     MOVE 002 TO KPS-KDCALL                                               
027300     MOVE UT19-KDPRODSL TO KPS-KDPRODSL                                   
027400     CALL WKPSKONV USING KPS-WKPSAREA                                     
027500     IF KPS-KDSVAR = SPACE                                                
027600       MOVE KPS-BEPRODSL-SVE TO UT19-BEPRODSL                             
027700     ELSE                                                                 
027800       MOVE SPACE TO UT19-BEPRODSL                                        
027900     END-IF                                                               
028000     .                                                                    
028100                                                                          
028200 Z-FINIT SECTION.                                                         
028300     SKIP2                                                                
028400     CLOSE W33013                                                         
028600           W33017                                                         
028700           W01160                                                         
028800           W33099                                                         
028900           W33019                                                         
029000     MOVE 'S' TO POSTSUM-OPKOD                                            
029100     CALL POSTSUM USING POSTSUM-PARM                                      
029200     .                                                                    
029300                                                                          
029400 S01-LAS-13-FIL SECTION.                                                  
029500     SKIP2                                                                
029600     PERFORM UNTIL INFIL-13-EOF = JA OR                                   
029700                   SPAR-IDARTNR < WS-IDARTNR                              
029800       READ W33013 INTO IN13-AREA                                         
029900         AT END MOVE JA TO INFIL-13-EOF                                   
030000       END-READ                                                           
030100       IF IN13-IDPTYP = '300'                                             
030200         MOVE IN13-IDARTNR TO WS-IDARTNR                                  
030300       END-IF                                                             
030400     END-PERFORM                                                          
030500     MOVE WS-IDARTNR TO SPAR-IDARTNR                                      
030600                                                                          
030700     IF INFIL-13-EOF = NEJ                                                
030800       MOVE '13'  TO POSTSUM-TRANSTYP                                     
030900       MOVE 'W33013' TO POSTSUM-FDNAMN                                    
031000       MOVE 'W33019D1' TO POSTSUM-DDNAMN2                                 
031100       CALL POSTSUM USING POSTSUM-PARM                                    
031200     END-IF                                                               
031300     .                                                                    
031400     EJECT                                                                
031500 S02-LAS-TABELL-FIL SECTION.                                              
031600     SKIP2                                                                
031700     READ W33099 INTO INTAB-AREA                                          
031800       AT END MOVE JA TO INFIL-BEFKNGRP-EOF                               
031900     END-READ                                                             
032000                                                                          
032100     IF INFIL-BEFKNGRP-EOF = NEJ                                          
032200       MOVE 'BE'  TO POSTSUM-TRANSTYP                                     
032300       MOVE 'W33099' TO POSTSUM-FDNAMN                                    
032400       MOVE 'W33019D5' TO POSTSUM-DDNAMN2                                 
032500       CALL POSTSUM USING POSTSUM-PARM                                    
032600     END-IF                                                               
032700     .                                                                    
032800     EJECT                                                                
034300 S17-LAS-17-FIL SECTION.                                                  
034400     SKIP2                                                                
034500     READ W33017 INTO IN17-AREA                                           
034600       AT END MOVE JA TO INFIL-17-EOF                                     
034700     END-READ                                                             
034800                                                                          
034900     IF INFIL-17-EOF = NEJ                                                
035000       MOVE '17'  TO POSTSUM-TRANSTYP                                     
035100       MOVE 'W33017' TO POSTSUM-FDNAMN                                    
035200       MOVE 'W33019D3' TO POSTSUM-DDNAMN2                                 
035300       CALL POSTSUM USING POSTSUM-PARM                                    
035400     END-IF                                                               
035500     .                                                                    
035600     EJECT                                                                
035700 S18-LAS-LB-FIL SECTION.                                                  
035800     SKIP2                                                                
035900     READ W01160 INTO LB-AREA                                             
036000       AT END MOVE JA TO INFIL-LB-EOF                                     
036100     END-READ                                                             
036200                                                                          
036300     IF INFIL-LB-EOF = NEJ                                                
036400       MOVE 'LB'  TO POSTSUM-TRANSTYP                                     
036500       MOVE 'W01160' TO POSTSUM-FDNAMN                                    
036600       MOVE 'W33019D4' TO POSTSUM-DDNAMN2                                 
036700       CALL POSTSUM USING POSTSUM-PARM                                    
036800     END-IF                                                               
036900     .                                                                    
037000     EJECT                                                                
037100 S19-SKRIV-19-FIL SECTION.                                                
037200     SKIP2                                                                
037300     WRITE UT-POST FROM UT19-AREA                                         
037400     MOVE '19'  TO POSTSUM-TRANSTYP                                       
037500     MOVE 'W33019' TO POSTSUM-FDNAMN                                      
037600     MOVE 'W33019D6' TO POSTSUM-DDNAMN2                                   
037700     CALL POSTSUM USING POSTSUM-PARM                                      
037800     .                                                                    
037900     EJECT                                                                
