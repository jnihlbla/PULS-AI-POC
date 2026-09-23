000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W2251400.                                    
000300 AUTHOR.                     IDK, 1978.                                   
000400 DATE-WRITTEN.               MAJ 1990.                                    
000500*****************************************************************         
000600*          ÄNDRAD TILL COBOL II DEN  5 MAJ -90                  *         
000700*               URBAN WENNBERG                                  *         
000800*****************************************************************         
000900     SKIP2                                                                
001000     REMARKS.                                                             
001100*    FUNKTION.                                                            
001200*        LISTNINGSPROGRAM FÖR RANKING PER VÄRDE, RADER OCH ÅLDER          
001300*        A.  SORTERAR FILEN W22515 PÅ CLAGER OCH VÄRDE                    
001400*        B.  PLOCKA UT 150 ST C1-ARTIKLAR OCH 50 ST C2-ARTIKLAR           
001500*            SKRIV RANKING-LISTAN TOTALT                                  
001600*            SKRIV 2 POSTER PER ARTIKEL PÅ EN ARBETSFIL                   
001700*        C.  SORTERAR ARBETSFILEN PÅ SEKTION, GRUPP, CLAGER               
001800*            OCH VÄRDE                                                    
001900*        D.  SKRIV RANKING-LISTAN PER GRUPP OCH PER SEKTION               
002000*        UPPREPA PUNKTERNA A-D FÖR RADER OCH ÅLDER                        
002100*    SKIP1                                                                
002200*        INNAN VARJE SORTERING UPPDATERAS W-STATUS MED                    
002300*        INFORMATION VILKET STEG SOM PÅBÖRJATS.                           
002400*    SKIP1                                                                
002500*        MODUL W200ANSK ANROPAS FÖR ATT ERHÅLLA GRUPPER                   
002600*        OCH SEKTIONER FRÅN IDANSK.                                       
002700*    SKIP2                                                                
002710*        RESTORDERLISTA RAD PÅ GRUPP OCH SEKTION BORTTAGEN                
002720*        AUG-SEPT 92. ERSATT MED SEPARAT LISTA W22524-001,                
002730*        W22524-002 OCH W22524-003. SK.                                   
002740*                                                                         
002800*    RETURKODER.                                                          
002900*        U0020       FEL I SORTERING                                      
003000     EJECT                                                                
003100 ENVIRONMENT DIVISION.                                                    
003110 CONFIGURATION SECTION.                                                   
003120 SPECIAL-NAMES.                                                           
003130     ALPHABET Y2000 IS X'05' THRU X'09' X'00' THRU X'04'.                 
003200 INPUT-OUTPUT SECTION.                                                    
003300 FILE-CONTROL.                                                            
003400     SKIP2                                                                
003500*--------------------------------------- LISTRECORD FÖR RANKING           
003600*                                        INPUT                            
003700     SELECT W22515 ASSIGN UT-S-W22514D1.                                  
003800*                                                                         
003900*--------------------------------------- RANKING LISTA                    
004000*                                                                         
004100     SELECT RA-LISTA ASSIGN UT-S-W22514D2.                                
004200*                                                                         
004300*--------------------------------------- ARBETSFIL TILL SORT 2            
004400*                                                                         
004500     SELECT W22521   ASSIGN UT-S-W22514D3.                                
004600*                                                                         
004700*--------------------------------------- SORTERINGSFIL SORT 1             
004800*                                                                         
004900     SELECT SRT1-FILE ASSIGN UT-S-W22514S1.                               
005000*                                                                         
005100*--------------------------------------- SORTERINGSFIL SORT 2             
005200*                                                                         
005300     SELECT SRT2-FILE ASSIGN UT-S-W22514S2.                               
005400     EJECT                                                                
005500 DATA DIVISION.                                                           
005600 FILE SECTION.                                                            
005700     SKIP2                                                                
005800 FD  W22515                                                               
005900     RECORDING F                                                          
006000     BLOCK 0                                                              
006100     LABEL RECORD STANDARD.                                               
006200*01  -COPY W225LI02    -L                                                 
006400     SKIP2                                                                
006500 FD  RA-LISTA                                                             
006600     RECORDING F                                                          
006700     LABEL RECORD STANDARD.                                               
006800 01  LISTPOST                  PIC X(122).                                
006900     SKIP2                                                                
007000 FD  W22521                                                               
007100     RECORDING F                                                          
007200     BLOCK 0                                                              
007300     LABEL RECORD STANDARD.                                               
007400 01  ARB-POST.                                                            
007500     03  ARB-IDSEKT         PIC S9(3)               COMP-3.               
007600     03  ARB-IDGRUPP        PIC S9(3)               COMP-3.               
007700*    03  ARB-W225LI02  -COPY W225LI02    -L                               
007900     SKIP2                                                                
008000 SD  SRT1-FILE                                                            
008100     RECORDING F.                                                         
008200*01  POST  -COPY W225LI02    -PRE SRT1-                                   
008210*Y2K-SORT                                                                 
008300     03  FILLER REDEFINES SRT1-W225LI02.                                  
008310       05  FILLER            PIC X(16).                                   
008320       05  SRT1-DECADE       PIC X.                                       
008330       05  SRT1-SMALL-DATE   PIC S9(3) COMP-3.                            
008400 SD  SRT2-FILE                                                            
008500     RECORDING F.                                                         
008600 01  SRT2-POST.                                                           
008700     03  SRT2-IDSEKT         PIC S9(3)               COMP-3.              
008800     03  SRT2-IDGRUPP        PIC S9(3)               COMP-3.              
008900*    03  -COPY W225LI02    -PRE SRT2-                                     
009000*Y2K-SORT                                                                 
009010     03  FILLER REDEFINES SRT2-W225LI02.                                  
009020       05  FILLER            PIC X(16).                                   
009030       05  SRT2-DECADE       PIC X.                                       
009040       05  SRT2-SMALL-DATE   PIC S9(3) COMP-3.                            
009100     EJECT                                                                
009200 WORKING-STORAGE SECTION.                                                 
009201                                                                          
009210*    -- CHECKED BY WY2000                                                 
009300 01  W.                                                                   
009400     05  W-PROGNAMN          PIC X(6)    VALUE 'W22514'.                  
009500     05  W-PROGSTATUS.                                                    
009600         10  FILLER          PIC X(12)   VALUE 'NU SORTERAS'.             
009700         10  W-RUB1-TXT1     PIC X(11).                                   
009800         10  W-RUB1-TXT2     PIC X(6).                                    
009900     05  W-RUB1-NR.                                                       
010000         10  FILLER          PIC X.                                       
010100         10  W-RUB1-NR-2     PIC X.                                       
010200     05  W-RUB1-IDSEKT       PIC 9(2).                                    
010300     05  W-ANT-C1            PIC S9(3)               COMP-3.              
010400     05  W-ANT-C2            PIC S9(3)               COMP-3.              
010500     05  W-TOPP              PIC X.                                       
010600     SKIP3                                                                
010700 01  RKOD                    PIC S9(4)   VALUE ZERO  COMP SYNC.           
010800     SKIP2                                                                
010900*-----------------------------------PARAMETRAR FÖR FILSLUT                
011000 01  SWITCHAR.                                                            
011100     05  SW-SRT1-EOF         PIC X       VALUE 'N'.                       
011200     05  SW-SRT2-EOF         PIC X       VALUE 'N'.                       
011300     EJECT                                                                
011400 01  FILLER                  PIC X(16)   VALUE ALL 'A'.                   
011500*-------------------------------------- PARAMETRAR TILL W200ANSK          
011600*                                                                         
011700*01  -COPY W009W42  -PRE  W-                                              
011900     EJECT                                                                
012000 01  FILLER                  PIC X(16)   VALUE ALL 'B'.                   
012100*--------------------------------------- AREA FÖR W22515-POST             
012200*                                                                         
012300 01  I15-AREA.                                                            
012400     03  I15-IDSEKT          PIC S9(3)              COMP-3.               
012500     03  I15-IDGRUPP         PIC S9(3)              COMP-3.               
012600*    03  -COPY W225LI02    -PRE   I15-                                    
012800     EJECT                                                                
012900 01  FILLER                  PIC X(16)   VALUE ALL 'C'.                   
013000 01  DYNAMISKA-SUBPROGRAM.                                                
013100     05  DATKORT             PIC X(8)    VALUE 'DATKORT '.                
013200     05  ABEND               PIC X(8)    VALUE 'ABEND   '.                
013300     05  W200ANSK            PIC X(8)    VALUE 'W200ANSK'.                
013400     SKIP2                                                                
013500*--------------------------------------- PARAMETRAR TILL DATKORT          
013600 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
013700*01  -COPY WDATKORT                                                       
013900     EJECT                                                                
014000*----------------------------------------PARAMETRAR FÖR PAGE COUNT        
014100 01  FILLER.                                                              
014200      03 W-SIDOR            PIC 9(3) VALUE 0.                             
014300 01  FILLER.                                                              
014400      03 W-RADRAKN          PIC 99 VALUE 1.                               
014500*---------------------------------------PARAMETER FÖR SIDBRYTNING         
014600 01  FILLER.                                                              
014700     03 W-ISEKT            PIC 9(2).                                      
014800*---------------------------------------------LISTRUBRIK 1                
014900 01  W-LIST-RUBRIK1.                                                      
015000      05  FILLER            PIC X(2) VALUE SPACE.                         
015100      05  FILLER            PIC X(15)   VALUE 'VOLVO PARTS'.              
015200      05  FILLER            PIC X(12)   VALUE 'W22514-001:'.              
015300      05  W-RUBRIK1-NR      PIC X(5).                                     
015400      05  FILLER            PIC X(15)    VALUE 'RANKING'.                 
015500      05  W-RUBRIK1-TXT1    PIC X(13).                                    
015600      05  W-RUBRIK1-IDSEKT  PIC Z(2).                                     
015700      05  FILLER            PIC X(2) VALUE SPACE.                         
015800      05  W-RUBRIK1-TXT2    PIC X(18).                                    
015900      05  FILLER             PIC X(5)    VALUE 'DATUM'.                   
016000      05  FILLER             PIC X(2) VALUE SPACE.                        
016100      05  W-RUBRIK1-D-AAR    PIC X(3).                                    
016200      05  W-RUBRIK1-D-MAANAD PIC X(3).                                    
016300      05  W-RUBRIK1-D-DAG    PIC X(3).                                    
016400      05  FILLER             PIC X(5) VALUE SPACE.                        
016500      05  FILLER             PIC X(4)    VALUE 'SID'.                     
016600      05  W-RUBRIK1-SID      PIC Z(3)9.                                   
016700     EJECT                                                                
016800*---------------------------------------------LISTRUBRIK 2                
016900 01  W-LIST-RUBRIK2.                                                      
017000      05  FILLER             PIC X(25) VALUE SPACE.                       
017100      05  FILLER             PIC X(11)    VALUE 'C  P T'.                 
017200      05  FILLER             PIC X(40)   VALUE                            
017300                 '***  R E S T O R D E R I N F O   ***'.                  
017400      05  FILLER             PIC X(43)   VALUE                            
017500                 '***    A N K O M S T I N F O   ***'.                    
017600     SKIP2                                                                
017700*---------------------------------------------LISTRUBRIK 3                
017800 01  W-LIST-RUBRIK3.                                                      
017900      05  FILLER             PIC X(7)  VALUE SPACE.                       
018000      05  FILLER             PIC X(7)  VALUE 'ARTNR'.                     
018100      05  FILLER             PIC X(4)  VALUE 'AG'.                        
018200      05  FILLER             PIC X(7)  VALUE 'LEVNR'.                     
018300      05  FILLER             PIC X(17) VALUE 'L  R 2'.                    
018400      05  FILLER             PIC X(10)  VALUE 'VÄRDE'.                    
018500      05  FILLER             PIC X(8)  VALUE 'RADER'.                     
018600      05  FILLER             PIC X(7)  VALUE 'ANTAL'.                     
018700      05  FILLER             PIC X(15) VALUE 'DATUM'.                     
018800      05  FILLER             PIC X(6)  VALUE 'VÄRDE'.                     
018900      05  FILLER             PIC X(9) VALUE 'I LAGRET'.                   
019000      05  FILLER             PIC X(10) VALUE 'TERMINAL'.                  
019100      05  FILLER             PIC X(14) VALUE 'PÅ VÄG'.                    
019200     EJECT                                                                
019300*----------------------------------------PARAMETRAR FÖR LISTRADER         
019400 01  W-SKRIVRAD.                                                          
019500      05  FILLER            PIC X(3) VALUE SPACE.                         
019600      05  W-SKRIV-IDARTNR   PIC Z(9).                                     
019700      05  FILLER            PIC X VALUE SPACE.                            
019800      05  W-SKRIV-IDANSK    PIC Z(3).                                     
019900      05  FILLER            PIC X(2) VALUE SPACE.                         
020000      05  W-SKRIV-IDLEVNR   PIC X(5).                                     
020100      05  FILLER            PIC X(2) VALUE SPACE.                         
020200      05  W-SKRIV-KDCLAGER  PIC Z.                                        
020300      05  FILLER            PIC X(2) VALUE SPACE.                         
020400      05  W-SKRIV-KDPRIO    PIC Z.                                        
020500      05  FILLER            PIC X VALUE SPACE.                            
020600      05  W-SKRIV-W-TOPP    PIC X.                                        
020700      05  FILLER            PIC X(4) VALUE SPACE.                         
020800      05  W-SKRIV-SUROBEL   PIC ZBZ(3)BZ(2)9.9(2)-.                       
020900      05  FILLER            PIC X(2) VALUE SPACE.                         
021000      05  W-SKRIV-KVRORAD   PIC Z(3)BZ(3)-.                               
021100      05  W-SKRIV-KVROS     PIC Z(3)BZ(3)-.                               
021200      05  FILLER            PIC X(2) VALUE SPACE.                         
021300      05  W-SKRIV-TIRODAT   PIC Z(4).                                     
021400      05  FILLER             PIC X(3) VALUE SPACE.                        
021500      05  W-SKRIV-SUAKBEL   PIC ZBZ(3)BZ(2)9.9(2)-.                       
021600      05  FILLER             PIC X VALUE SPACE.                           
021700      05  W-SKRIV-KVAKS-CDC PIC Z(3)BZ(3)-.                               
021800      05  FILLER             PIC X VALUE SPACE.                           
021900      05  W-SKRIV-KVAKS-T   PIC Z(3)BZ(3)-.                               
022000      05  W-SKRIV-KVAKS-PAV PIC Z(3)BZ(3)-.                               
022100                                                                          
022200*--------------------------------------- BLANK LISTRAD                    
022300 01  FILLER.                                                              
022400     03  W-BLANKRAD          PIC X(122) VALUE SPACE.                      
022500     EJECT                                                                
022600                                                                          
022700 PROCEDURE DIVISION.                                                      
022800******************************************************************        
022900*    SORTERAR OCH SKRIVER UT LISTA                                        
023000******************************************************************        
023100 STYR SECTION.                                                            
023200     SKIP2                                                                
023300     PERFORM A-INITIERING                                                 
023400     SKIP1                                                                
023500     MOVE 'TOTALT' TO W-RUB1-TXT1                                         
023600     MOVE 'VÄRDE' TO W-RUB1-TXT2                                          
023700     MOVE '11' TO W-RUB1-NR                                               
023800     MOVE ZERO TO W-RUB1-IDSEKT                                           
023900     SKIP2                                                                
024000*---------------------------------------- SORTERAR W22515 PÅ VÄRDE        
024100     SORT SRT1-FILE                                                       
024200         ASCENDING SRT1-KDCLAGER                                          
024300         DESCENDING SRT1-SUROBEL                                          
024400         ASCENDING SRT1-IDARTNR                                           
024500         USING W22515                                                     
024600         OUTPUT PROCEDURE C-SKRIV-LISTA-ARBFIL                            
024700     SKIP1                                                                
024800     IF SORT-RETURN > +0                                                  
024900         DISPLAY '*** W22514, FEL VID SORTERING 1 (VÄRDE)'                
025000         MOVE +20 TO RKOD                                                 
025100         CALL ABEND USING RKOD                                            
025200     END-IF                                                               
025300     EJECT                                                                
025400     MOVE 'PER GRUPP' TO W-RUB1-TXT1                                      
025500     MOVE '13' TO W-RUB1-NR                                               
025600     SKIP2                                                                
025700*--------------------------------------- SORTERAR ARBETSFIL PÅ VÄR        
025800     SORT SRT2-FILE                                                       
025900        ASCENDING SRT2-IDSEKT SRT2-IDGRUPP SRT2-KDCLAGER                  
026000        DESCENDING SRT2-SUROBEL                                           
026100        ASCENDING  SRT2-IDARTNR                                           
026200        USING W22521                                                      
026300        OUTPUT PROCEDURE D-SKRIV-LISTA-GRP-SEK                            
026400     SKIP1                                                                
026500     IF SORT-RETURN > +0                                                  
026600         DISPLAY '*** W22514, FEL I SORTERING 2 (VÄRDE)'                  
026700         MOVE +20 TO RKOD                                                 
026800         CALL ABEND USING RKOD                                            
026900     END-IF                                                               
027000     EJECT                                                                
027100     MOVE 'TOTALT' TO W-RUB1-TXT1                                         
027200     MOVE 'RADER' TO W-RUB1-TXT2                                          
027300     MOVE '21' TO W-RUB1-NR                                               
027400     MOVE ZERO TO W-RUB1-IDSEKT                                           
027500     SKIP2                                                                
027600*---------------------------------------- SORTERAR W22515 PÅ RADER        
027700     SORT SRT1-FILE                                                       
027800         ASCENDING SRT1-KDCLAGER                                          
027900         DESCENDING SRT1-KVRORAD                                          
028000         ASCENDING SRT1-IDARTNR                                           
028100         USING W22515                                                     
028200         OUTPUT PROCEDURE C-SKRIV-LISTA-ARBFIL                            
028300     SKIP1                                                                
028400     IF SORT-RETURN > +0                                                  
028500         DISPLAY '*** W22514, FEL VID SORTERING 3 (RADER)'                
028600         MOVE +20 TO RKOD                                                 
028700         CALL ABEND USING RKOD                                            
028800     END-IF                                                               
028900     EJECT                                                                
028910*                                                                         
028920*    ÄT AUG 92                                                            
028930*    RESTORDERLISTA SORTERAD PÅ RAD MED UPPDELNING PÅ                     
028940*    SEKTION / ANSK-GRUPP / ANSKAFFARE ÖVERFLYTTAD                        
028950*    TILL W225P024, PGM R.PROD.EPLUS.W22524.                              
028960*                                                                         
029000*    MOVE 'PER GRUPP' TO W-RUB1-TXT1                                      
029100*    MOVE '23' TO W-RUB1-NR                                               
029200*    SKIP2                                                                
029300*--------------------------------------- SORTERAR ARBETSFIL PÅ RAD        
029400*    SORT SRT2-FILE                                                       
029500*       ASCENDING SRT2-IDSEKT SRT2-IDGRUPP SRT2-KDCLAGER                  
029600*       DESCENDING SRT2-KVRORAD                                           
029700*       ASCENDING  SRT2-IDARTNR                                           
029800*       USING W22521                                                      
029900*       OUTPUT PROCEDURE D-SKRIV-LISTA-GRP-SEK                            
030000*    SKIP1                                                                
030100*    IF SORT-RETURN > +0                                                  
030200*        DISPLAY '*** W22514, FEL I SORTERING 4 (RADER)'                  
030300*        MOVE +20 TO RKOD                                                 
030400*        CALL ABEND USING RKOD                                            
030500*    END-IF                                                               
030600*    EJECT                                                                
030610                                                                          
030700     MOVE 'TOTALT' TO W-RUB1-TXT1                                         
030800     MOVE 'ÅLDER' TO W-RUB1-TXT2                                          
030900     MOVE '31' TO W-RUB1-NR                                               
031000     MOVE ZERO TO W-RUB1-IDSEKT                                           
031100     SKIP2                                                                
031200*---------------------------------------- SORTERAR W22515 PÅ ÅLDER        
031300     SORT SRT1-FILE                                                       
031400         ASCENDING SRT1-KDCLAGER                                          
031410                   SRT1-DECADE                                            
031420                   SRT1-SMALL-DATE                                        
031430                   SRT1-IDARTNR                                           
031440         COLLATING SEQUENCE Y2000                                         
031500         USING W22515                                                     
031600         OUTPUT PROCEDURE C-SKRIV-LISTA-ARBFIL                            
031700     SKIP1                                                                
031800     IF SORT-RETURN > +0                                                  
031900         DISPLAY '*** W22514, FEL VID SORTERING 5 (ÅLDER)'                
032000         MOVE +20 TO RKOD                                                 
032100         CALL ABEND USING RKOD                                            
032200     END-IF                                                               
032300     EJECT                                                                
032400     MOVE 'PER GRUPP' TO W-RUB1-TXT1                                      
032500     MOVE '33' TO W-RUB1-NR                                               
032600     SKIP2                                                                
032700*--------------------------------------- SORTERAR ARBETSFIL PÅ ÅLD        
032800     SORT SRT2-FILE                                                       
032900         ASCENDING SRT2-IDSEKT                                            
032910                   SRT2-IDGRUPP                                           
032920                   SRT2-KDCLAGER                                          
033000                   SRT2-DECADE                                            
033001                   SRT2-SMALL-DATE                                        
033010                   SRT2-IDARTNR                                           
033020        COLLATING SEQUENCE Y2000                                          
033100        USING W22521                                                      
033200        OUTPUT PROCEDURE D-SKRIV-LISTA-GRP-SEK                            
033300     SKIP1                                                                
033400     IF SORT-RETURN > +0                                                  
033500         DISPLAY '*** W22514, FEL I SORTERING 6 (ÅLDER)'                  
033600         MOVE +20 TO RKOD                                                 
033700         CALL ABEND USING RKOD                                            
033800     END-IF                                                               
033900     SKIP1                                                                
034000     PERFORM Z-AVSLUTNING                                                 
034100     GOBACK                                                               
034200     .                                                                    
034300     EJECT                                                                
034400 A-INITIERING SECTION.                                                    
034500******************************************************************        
034600*    ÖPPNA LISTAN                                                *        
034700******************************************************************        
034800     SKIP2                                                                
034900     OPEN OUTPUT RA-LISTA                                                 
035000     SKIP1                                                                
035100     CALL DATKORT USING W-PROGNAMN DATUMKORT-ID DATUMKORT                 
035200     .                                                                    
035300     EJECT                                                                
035400                                                                          
035500                                                                          
035600 C-SKRIV-LISTA-ARBFIL SECTION.                                            
035700******************************************************************        
035800*    SKRIVER LISTA TOTALT OCH SKRIVER ARBETSFIL                  *        
035900******************************************************************        
036000     SKIP2                                                                
036100     MOVE 'N' TO SW-SRT1-EOF                                              
036200     MOVE 1 TO W-ANT-C1                                                   
036300     MOVE 1 TO W-ANT-C2                                                   
036400     SKIP1                                                                
036500     MOVE +40 TO W-RADRAKN                                                
036600     PERFORM CB-LAS-SRTFIL1                                               
036700     OPEN OUTPUT W22521                                                   
036800     SKIP1                                                                
036900*--------------------------------------- PLOCKA UT 150 FRÅN               
037000*                                        C1LAGER                          
037100     SKIP1                                                                
037200         PERFORM UNTIL W-ANT-C1 > 150 OR SW-SRT1-EOF = 'J' OR             
037300                    I15-KDCLAGER NOT = 1                                  
037400     SKIP1                                                                
037410            IF W-RUB1-NR NOT = '21'                                       
037500               PERFORM CA-SKRIV-ARBFIL2                                   
037510            END-IF                                                        
037600            PERFORM S01-SKRIV-LISTA-RA                                    
037700            PERFORM CB-LAS-SRTFIL1                                        
037800            ADD 1 TO W-ANT-C1                                             
037900         END-PERFORM                                                      
038000     SKIP1                                                                
038100*--------------------------------------- LÄS FÖRBI RESTEN AV              
038200*                                        C1-POSTERNA                      
038300         PERFORM UNTIL I15-KDCLAGER NOT = 1 OR SW-SRT1-EOF = 'J'          
038400             PERFORM CB-LAS-SRTFIL1                                       
038500         END-PERFORM                                                      
038600     SKIP2                                                                
038700*--------------------------------------- PLOCKA UT 50 FRÅN C2LAGER        
038800*                                                                         
040400         CLOSE W22521                                                     
040500      .                                                                   
040600     EJECT                                                                
040700                                                                          
040800                                                                          
040900                                                                          
041000 CA-SKRIV-ARBFIL2 SECTION.                                                
041100******************************************************************        
041200*    HÄMTAR GRUPP OCH SEKTION MED HJÄLP AV W200ANSK.             *        
041300*    SKRIVER EN POST MED NOLL I SECTION OCH EN MED               *        
041400*    NOLL I GRUPP.                                               *        
041500******************************************************************        
041600     SKIP2                                                                
041700     MOVE I15-IDANSK TO W-IDANSK                                          
041800     CALL W200ANSK USING W-W009W42                                        
041900     SKIP1                                                                
042000     MOVE ZERO TO I15-IDSEKT                                              
042100     MOVE W-IDGRUPP TO I15-IDGRUPP                                        
042200     WRITE ARB-POST FROM I15-AREA                                         
042300     SKIP1                                                                
042400     MOVE W-IDSEKT TO I15-IDSEKT                                          
042500     MOVE ZERO TO I15-IDGRUPP                                             
042600     WRITE ARB-POST FROM I15-AREA                                         
042700     .                                                                    
042800     EJECT                                                                
042900                                                                          
043000                                                                          
043100                                                                          
043200 CB-LAS-SRTFIL1 SECTION.                                                  
043300******************************************************************        
043400*    LÄSER SORTFIL 1                                             *        
043500******************************************************************        
043600     SKIP2                                                                
043700     RETURN SRT1-FILE INTO I15-W225LI02                                   
043800         END MOVE 'J' TO SW-SRT1-EOF                                      
043900         .                                                                
044000     EJECT                                                                
044100 D-SKRIV-LISTA-GRP-SEK SECTION.                                           
044200******************************************************************        
044300*    SKRIVER LISTAN PER GRUPP OCH SEKTION                        *        
044400******************************************************************        
044500     SKIP2                                                                
044600     MOVE 'N' TO SW-SRT2-EOF                                              
044700     MOVE +40 TO W-RADRAKN                                                
044800         PERFORM DA-LAS-SRTFIL2                                           
044900     SKIP1                                                                
045000         PERFORM UNTIL SW-SRT2-EOF = 'J'                                  
045100     SKIP1                                                                
045200             IF  I15-IDSEKT  = ZERO                                       
045300                 MOVE I15-IDGRUPP TO W-RUB1-IDSEKT                        
045400             ELSE                                                         
045500                 MOVE 'PER SEKTION' TO W-RUB1-TXT1                        
045600                 MOVE I15-IDSEKT TO W-RUB1-IDSEKT                         
045700                 MOVE '2' TO W-RUB1-NR-2                                  
045800             END-IF                                                       
045900     SKIP1                                                                
046000             PERFORM S01-SKRIV-LISTA-RA                                   
046100             PERFORM DA-LAS-SRTFIL2                                       
046200         END-PERFORM                                                      
046300      .                                                                   
046400     EJECT                                                                
046500 DA-LAS-SRTFIL2 SECTION.                                                  
046600******************************************************************        
046700*    LÄSER SORTFIL 2                                             *        
046800******************************************************************        
046900     SKIP2                                                                
047000     RETURN SRT2-FILE INTO I15-AREA                                       
047100         END MOVE 'J' TO SW-SRT2-EOF                                      
047200     SKIP1                                                                
047300     .                                                                    
047400     EJECT                                                                
047500                                                                          
047600                                                                          
047700                                                                          
047800 S01-SKRIV-LISTA-RA SECTION.                                              
047900******************************************************************        
048000*    SKRIV LISTA RANKING                                         *        
048100******************************************************************        
048200     SKIP2                                                                
048300     ADD +1 TO W-RADRAKN                                                  
048400     IF I15-FLTOPP = 'J'                                                  
048500         MOVE '*' TO W-TOPP                                               
048600     ELSE                                                                 
048700         MOVE SPACE TO W-TOPP                                             
048800     END-IF                                                               
048900     IF W-RADRAKN > 36 OR W-RUB1-IDSEKT NOT = W-ISEKT                     
049000           PERFORM F-LISTRUBRIK                                           
049100     END-IF                                                               
049200     PERFORM E-FLYTTA-TXT                                                 
049300     WRITE LISTPOST FROM W-SKRIVRAD                                       
049400     .                                                                    
049500     EJECT                                                                
049600                                                                          
049700 E-FLYTTA-TXT SECTION.                                                    
049800******************************************************************        
049900*     FLYTTA LISTFÄLTEN TILL RESP. FÄLT                          *        
050000******************************************************************        
050100     MOVE I15-IDARTNR TO W-SKRIV-IDARTNR                                  
050200     MOVE I15-IDANSK TO W-SKRIV-IDANSK                                    
050300     MOVE I15-IDLEVNR TO W-SKRIV-IDLEVNR                                  
050400     MOVE I15-KDCLAGER TO W-SKRIV-KDCLAGER                                
050500     MOVE I15-KDPRIO TO W-SKRIV-KDPRIO                                    
050600     MOVE W-TOPP TO W-SKRIV-W-TOPP                                        
050700     MOVE I15-SUROBEL TO W-SKRIV-SUROBEL                                  
050800     MOVE I15-KVRORAD TO W-SKRIV-KVRORAD                                  
050900     MOVE I15-KVROS TO W-SKRIV-KVROS                                      
051000     MOVE I15-TIRODAT-ORDER TO W-SKRIV-TIRODAT                            
051100     MOVE I15-SUAKBEL TO W-SKRIV-SUAKBEL                                  
051200     MOVE I15-KVAKS-CDC  TO W-SKRIV-KVAKS-CDC                             
051300     MOVE I15-KVAKS-PAV  TO W-SKRIV-KVAKS-PAV                             
051400     MOVE I15-KVAKS-T    TO W-SKRIV-KVAKS-T                               
051500     .                                                                    
051600     EJECT                                                                
051700                                                                          
051800 F-LISTRUBRIK SECTION.                                                    
051900******************************************************************        
052000*     SKRIVA UT LIST RUBRIKER                                    *        
052100******************************************************************        
052200          MOVE ZERO TO W-RADRAKN                                          
052300          ADD 1 TO W-SIDOR                                                
052400          MOVE W-RUB1-NR TO W-RUBRIK1-NR                                  
052500          MOVE W-RUB1-TXT1 TO W-RUBRIK1-TXT1                              
052600          MOVE W-RUB1-IDSEKT TO W-RUBRIK1-IDSEKT                          
052700          MOVE W-RUB1-TXT2 TO W-RUBRIK1-TXT2                              
052800          MOVE W-RUB1-IDSEKT TO W-ISEKT                                   
052900          MOVE D-AAR    TO W-RUBRIK1-D-AAR                                
053000          MOVE D-MAANAD TO W-RUBRIK1-D-MAANAD                             
053100          MOVE D-DAG    TO W-RUBRIK1-D-DAG                                
053200          MOVE W-SIDOR TO W-RUBRIK1-SID                                   
053300          WRITE LISTPOST FROM W-LIST-RUBRIK1 AFTER PAGE                   
053400          WRITE LISTPOST FROM W-BLANKRAD                                  
053500          WRITE LISTPOST FROM W-LIST-RUBRIK2                              
053600          WRITE LISTPOST FROM W-LIST-RUBRIK3                              
053700          WRITE LISTPOST FROM W-BLANKRAD                                  
053800          .                                                               
053900     EJECT                                                                
054000                                                                          
054100                                                                          
054200 Z-AVSLUTNING SECTION.                                                    
054300******************************************************************        
054400*    STÄNG LISTAN                                                *        
054500******************************************************************        
054600     SKIP2                                                                
054700     CLOSE RA-LISTA                                                       
054800     .                                                                    
054900     EJECT                                                                
055000                                                                          
055100                                                                          
