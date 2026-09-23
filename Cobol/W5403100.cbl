000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W5403100.                                                
000400 AUTHOR.         KARL JOHAN HANSSON.                                      
000500 DATE-WRITTEN.   96/11/29.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER WDK6 MED SB OCH SORTERAR FILEN PÅ ARTIKELNR                
001000*                                                                         
001100*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  FEL I SORTEN                                            
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*          --- ARTIKELINFO                                                
002400     SELECT W54031                     ASSIGN TO W54031D1.                
002500     SKIP2                                                                
002600*          --- SORTERINGSFIL                                              
002700     SELECT SORTFIL                    ASSIGN TO W54031DS.                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  W54031                                                               
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600     SKIP2                                                                
003700*01  POST -COPY W54031 -PRE  SORTWS- -L.                                  
003800     SKIP3                                                                
003900 SD  SORTFIL.                                                             
004000     SKIP2                                                                
004100 01  SRT-POST.                                                            
004200     03  -COPY W54031 -PRE  SRT-                                          
004300     EJECT                                                                
004400 WORKING-STORAGE SECTION.                                                 
004500                                                                          
004501                                                                          
004510*    -- CHECKED BY WY2000                                                 
004600 77  IDPGM                       PIC X(8)    VALUE 'W5403100'.            
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900 77  INDX                        PIC 9       VALUE ZERO.                  
005000 77  SPAR-TIPRLIST               PIC S9(7)   VALUE ZERO COMP-3.           
005100                                                                          
005200 77  SKRIV-SW                    PIC X       VALUE 'N'.                   
005300     88  SKRIV-POST                          VALUE 'J'.                   
005400                                                                          
005500 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
005600     88  END-OF-SORTFIL                      VALUE 'J'.                   
006300     EJECT                                                                
006400 01  DYNAMISKA-SUBPROGRAM.                                                
006500*                                                                         
006600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007000     SKIP2                                                                
007100*    --- PARAMETRAR TILL ABEND                                            
007200                                                                          
007300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007500     SKIP2                                                                
007600 01  FELTEXT.                                                             
007700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007900     EJECT                                                                
008000*    --- PARAMETRAR TILL POSTSUM                                          
008100*                                                                         
008200*01  -COPY W0005   -PRE  POSTSUM-                                         
008300     EJECT                                                                
008400 01  UT-AREA-START               PIC X(24)   VALUE                        
008500                                 'UT-AREA-START  '.                       
008600     SKIP2                                                                
008700                                                                          
008800*01  AREA -COPY W54031     -PRE UT-                                       
008900 01  SORT-AREA-START             PIC X(24)   VALUE                        
009000                                 'SORT-AREA-START'.                       
009300*01  AREA -COPY W54031     -PRE SORTWS-                                   
009400 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
009500     EJECT                                                                
009600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010000     SKIP3                                                                
010100 01  NYCKLAR-TILL-DLI.                                                    
010200     03  W-IDARTNR-X.                                                     
010300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010400     SKIP2                                                                
010500*    --- STATUS-KOD FRÅN IMS                                              
010600 01  STATUS-WS                   PIC XX.                                  
010700     88  SEGMENT-FINNS                       VALUE '  '.                  
010800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010900     SKIP2                                                                
011000 01  GODK-STATUSKODER.                                                    
011100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011200     SKIP3                                                                
011300 01  SSA1                        PIC X(64).                               
011400 01  SSA2                        PIC X(64).                               
011500     EJECT                                                                
011600*    --- IMS FUNKTIONSKODER                                               
011700*01  -COPY W0003                                                          
011800     EJECT                                                                
011900*    ---  DLI INPUT-OUTPUT AREA                                           
012000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
012100     SKIP3                                                                
012200 01  DLI-IO-AREA.                                                         
012300     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
012400     SKIP3                                                                
012500     03  WLARTC01 REDEFINES IO-AREA.                                      
012600*        05  -COPY WDK601                                                 
012700     EJECT                                                                
012800     03  WLARTC12 REDEFINES IO-AREA.                                      
012900*        05  -COPY WDK611                                                 
013300     EJECT                                                                
013400 LINKAGE SECTION.                                                         
013500                                                                          
013600     EJECT                                                                
013700*01  -COPY W0008  -PRE ARTC-                                              
013800     05  FILLER                  PIC X.                                   
013900     EJECT                                                                
014000 PROCEDURE DIVISION  USING ARTC-PCB.                                      
014010 MAIN SECTION.                                                            
014100     ENTRY 'DLITCBL' USING ARTC-PCB.                                      
014200                                                                          
014300                                                                          
014400     PERFORM A-INIT                                                       
014500                                                                          
014600     SORT SORTFIL ASCENDING KEY SRT-IDARTNR                               
014700                  INPUT PROCEDURE B-LAES-INPUT                            
014800                  GIVING W54031                                           
014900                                                                          
015000     IF SORT-RETURN NOT = 0                                               
015100       MOVE SORT-RETURN TO SORT-RETURN-X                                  
015200       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
015300           DELIMITED BY SIZE                                              
015400           INTO FELTEXT-STR                                               
015500       DISPLAY FELTEXT                                                    
015600       PERFORM S99-ABEND                                                  
015700     ELSE                                                                 
015800       PERFORM Z-FINIT                                                    
015900                                                                          
016000       MOVE ZERO TO RETURN-CODE                                           
016100       GOBACK                                                             
016200     END-IF                                                               
016300                                                                          
016400     .                                                                    
016500     EJECT                                                                
016600 A-INIT SECTION.                                                          
016700                                                                          
017000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017500     PERFORM AA-NOLLSTALL-UTPOST                                          
017501     .                                                                    
017502     SKIP3                                                                
017510 AA-NOLLSTALL-UTPOST SECTION.                                             
017520                                                                          
017600     MOVE ZERO  TO SORTWS-IDARTNR                                         
017800                   SORTWS-BEFT                                            
017900                   SORTWS-KDVTH                                           
018600                   SORTWS-PRDIRLON                                        
018700                   SORTWS-PRDMTRL                                         
018800                   SORTWS-PROVRPAL                                        
018900     MOVE SPACE TO SORTWS-IDLEVNR                                         
023700     .                                                                    
023800     EJECT                                                                
023900 B-LAES-INPUT SECTION.                                                    
024000                                                                          
024100     PERFORM IMS-GET-WDK6                                                 
024200     PERFORM UNTIL SEGMENT-SLUT                                           
024210       IF ARTC-SEG-NAME-FB = 'WDK601  '                                   
024500         IF SKRIV-POST                                                    
024600           PERFORM S31-RELEASE-W54031                                     
024700           PERFORM AA-NOLLSTALL-UTPOST                                    
024800         END-IF                                                           
024900         IF ART-KDERS-UTG = ZERO                                          
025100           MOVE +1 TO INDX                                                
025200           MOVE ART-IDARTNR        TO SORTWS-IDARTNR                      
025300           MOVE ART-IDLEVNR        TO SORTWS-IDLEVNR                      
025700           MOVE JA TO SKRIV-SW                                            
026100         ELSE                                                             
026200           MOVE NEJ TO SKRIV-SW                                           
026210         END-IF                                                           
026300       ELSE                                                               
026301         IF ARTC-SEG-NAME-FB = 'WDK611  '                                 
026500           IF SKRIV-POST                                                  
027100             MOVE CLAG-BEFT        TO SORTWS-BEFT                         
027200             MOVE CLAG-KDVTH       TO SORTWS-KDVTH                        
027500             MOVE CLAG-PRDIRLON    TO SORTWS-PRDIRLON                     
027600             MOVE CLAG-PRDMTRL     TO SORTWS-PRDMTRL                      
027700             MOVE CLAG-PROVRPAL    TO SORTWS-PROVRPAL                     
028700           END-IF                                                         
028800         END-IF                                                           
028900       END-IF                                                             
030200       PERFORM IMS-GET-WDK6                                               
030300     END-PERFORM                                                          
030400     IF SKRIV-POST                                                        
030500       PERFORM S31-RELEASE-W54031                                         
030600       PERFORM AA-NOLLSTALL-UTPOST                                        
030700     END-IF                                                               
030800     .                                                                    
030900     EJECT                                                                
031000 Z-FINIT SECTION.                                                         
031100                                                                          
031200     MOVE 'S' TO POSTSUM-OPKOD                                            
031300     CALL POSTSUM USING POSTSUM-PARM                                      
031400     .                                                                    
031500     SKIP3                                                                
031600 S31-RELEASE-W54031 SECTION.                                              
031700                                                                          
031800     RELEASE SRT-POST FROM SORTWS-AREA                                    
031900                                                                          
032000     MOVE 'WDK6'    TO POSTSUM-TRANSTYP                                   
032100     MOVE 'W54031' TO POSTSUM-FDNAMN                                      
032200     MOVE 'W54031D1' TO POSTSUM-DDNAMN2                                   
032300     CALL POSTSUM USING POSTSUM-PARM                                      
032400     .                                                                    
032500     SKIP3                                                                
032600 S99-ABEND SECTION.                                                       
032700                                                                          
032800     MOVE 'S' TO POSTSUM-OPKOD                                            
032900     CALL POSTSUM USING POSTSUM-PARM                                      
033000     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
033100     .                                                                    
033200     EJECT                                                                
033300* --- IMS SEKTIONER ---                                                   
033400                                                                          
033600 IMS-GET-WDK6   SECTION.                                                  
033700                                                                          
033800     CALL CBLTDLI USING GN ARTC-PCB DLI-IO-AREA                           
033900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
034000     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
034100     PERFORM IMS-STATUSKONTROLL                                           
034200     .                                                                    
034300     EJECT                                                                
034400 IMS-STATUSKONTROLL SECTION.                                              
034500                                                                          
034600     SET STATUS-IX TO 1                                                   
034700     SEARCH GODK-STATUS                                                   
034800       AT END                                                             
034900         MOVE 'EJ GODK STATUS' TO FELTEXT-STR                             
035000         DISPLAY FELTEXT                                                  
035100         CALL FELLOG                                                      
035200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
035300         CONTINUE                                                         
035400     END-SEARCH                                                           
035500     .                                                                    
