000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5410500.                                                
000300 AUTHOR.         RANDI BERG.                                              
000400 DATE-WRITTEN.   98/08/31.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        PROGRAMMET LÄSER PEDALFIL OCH SKAPAR FIL FÖR LIST-PROGRAM        
001000*        I FAKTURERINGEN.                                                 
001100*        ALLA POSTER MED HUVUD-HÄNDELSE 201, 202, 203, 304 OCH 501        
001200*             SKAPAR W54105 MED RADPOSTER TYP 942                         
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- INFIL FRÅN PEDAL                                           
002700     SELECT W51067                     ASSIGN TO W54105D1.                
002800     SKIP2                                                                
002900*          --- UTFIL MED ALLA FAKTURERINGSPOSTER                          
003000     SELECT W54105                     ASSIGN TO W54105D2.                
003100     SKIP2                                                                
003200*          --- SORTERINGSFIL                                              
003300     SELECT SORTFIL                    ASSIGN TO W54105DS.                
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP3                                                                
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
003900 FD  W51067                                                               
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004300*01  -COPY W51060      -L.                                                
004400     SKIP3                                                                
004500 FD  W54105                                                               
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS  0.                                                   
004800                                                                          
004900*01  POST -COPY W510942 -PRE  UTALL-942- -L.                              
005000     SKIP2                                                                
005100 SD  SORTFIL.                                                             
005200                                                                          
005300*01  POST -COPY W51060      -PRE SORT-                                    
005400     EJECT                                                                
005500 WORKING-STORAGE SECTION.                                                 
005600                                                                          
005700 77  IDPGM                       PIC X(8)    VALUE 'W5410500'.            
005800 77  JA                          PIC X       VALUE 'J'.                   
005900 77  NEJ                         PIC X       VALUE 'N'.                   
006000                                                                          
006100 77  W51067-EOF-SW               PIC X       VALUE 'N'.                   
006200     88  END-OF-W51067                       VALUE 'J'.                   
006300                                                                          
006400 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
006500                                                                          
006600 01  W-IDVERGL               PIC X(10).                                   
006700 01  W-IDFAKT-X              PIC X(7).                                    
006800 01  W-IDFAKT                PIC S9(7).                                   
006900                                                                          
007000 01  W-SUARTSTD                PIC S9(9)V9(2)   VALUE ZERO.               
007100 01  W-SUARTSJK                PIC S9(9)V9(2)   VALUE ZERO.               
007200 01  W-SUARTNTO                PIC S9(9)V9(2)   VALUE ZERO.               
007300     EJECT                                                                
007400 01  DYNAMISKA-SUBPROGRAM.                                                
007500*                                                                         
007600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008000                                                                          
008100*    --- PARAMETRAR TILL ABEND                                            
008200                                                                          
008300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008600     SKIP2                                                                
008700 01  FELTEXT.                                                             
008800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009000     EJECT                                                                
009100*    --- VALID IDDC CODES                                                 
009200*                                                                         
009300*01  -COPY WWDC99                                                         
009400*    --- PARAMETRAR TILL POSTSUM                                          
009500*                                                                         
009600*01  -COPY W0005   -PRE  POSTSUM-                                         
009700     EJECT                                                                
009800 01  FILLER                      PIC X(24)   VALUE 'IN-AREA'.             
009900                                                                          
010000 01  IN-AREA.                                                             
010100*    03  FILLER    -COPY W51060   -PRE IN-                                
010200     EJECT                                                                
010300 01  FILLER                      PIC X(24)   VALUE 'UT-AREA'.             
010400                                                                          
010500 01  UT-AREA.                                                             
010600*    03  AREA -COPY W510942  -PRE UT942-                                  
010700     EJECT                                                                
010800 01  FILLER                      PIC X(24)   VALUE  'SORTWS-AREA'.        
010900                                                                          
011000*01  AREA -COPY W51060      -PRE SORTWS-                                  
011100 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
011200     EJECT                                                                
011300*--------------------------------------- NYCKLAR TILL BASERNA             
011400                                                                          
011500 01  W-IDDC-B6-X.                                                         
011600     03 W-IDDC-B6        PIC X(2).                                        
011700                                                                          
011800     EJECT                                                                
011900*--------------------------------------- ARBETSAREOR TILL                 
012000*                                        IMS-SEKTIONERNA                  
012100 01      IMS-WS.                                                          
012200   03    FILLER          PIC X(8)    VALUE 'IMS-WS  '.                    
012300     SKIP3                                                                
012400*--------------------------------------- STATUSKOD FRÅN IMS               
012500   03    STATUS-WS       PIC XX.                                          
012600     88  SEGMENT-FINNS               VALUE '  '.                          
012700     88  SEGMENT-SAKNAS              VALUE 'GE'.                          
012800     88  SEGMENT-FINNS-REDAN         VALUE 'II'.                          
012900     88  IMS-EJ-OK                   VALUE 'XD'.                          
013000     SKIP3                                                                
013100   03    SSA1            PIC X(128).                                      
013200                                                                          
013300                                                                          
013400   03    GODK-STATUSKODER.                                                
013500     05  GODK-STATUS OCCURS 10 INDEXED BY STATUS-IX PIC XX.               
013600                                                                          
013700     EJECT                                                                
013800*01      -COPY W0003                                                      
013900                                                                          
014000 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
014100 01   DLI-IO-AREA-B601.                                                   
014200*     03  -COPY WDB601                                                    
014300                                                                          
014400 LINKAGE SECTION.                                                         
014500*01  -COPY W0008 -PRE WDB6-                                               
014600     05  FILLER           PIC X.                                          
014700                                                                          
014800     EJECT                                                                
014900 PROCEDURE DIVISION USING WDB6-PCB.                                       
015000                                                                          
015100 MAIN SECTION.                                                            
015200     ENTRY 'DLITCBL' USING WDB6-PCB.                                      
015300                                                                          
015400     PERFORM A-INIT                                                       
015500                                                                          
015600     SORT SORTFIL ASCENDING KEY SORT-EKHT-IDVERGL                         
015700                                SORT-EKHT-KDEKNIVA                        
015800                  INPUT PROCEDURE B-SORT-INPUT                            
015900                  OUTPUT PROCEDURE C-SORT-OUTPUT                          
016000                                                                          
016100     IF SORT-RETURN NOT = 0                                               
016200       MOVE SORT-RETURN TO SORT-RETURN-X                                  
016300       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
016400       DELIMITED BY SIZE INTO FELTEXT-STR                                 
016500       DISPLAY FELTEXT                                                    
016600       MOVE RKOD-ABEND-UTAN-DUMP TO RKOD-ABEND                            
016700       PERFORM S99-ABEND                                                  
016800     ELSE                                                                 
016900       PERFORM Z-FINIT                                                    
017000                                                                          
017100       MOVE ZERO TO RETURN-CODE                                           
017200       GOBACK                                                             
017300     END-IF                                                               
017400                                                                          
017500     .                                                                    
017600     EJECT                                                                
017700 A-INIT SECTION.                                                          
017800                                                                          
017900     OPEN INPUT  W51067                                                   
018000          OUTPUT W54105                                                   
018100                                                                          
018200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018300     .                                                                    
018400     EJECT                                                                
018500 B-SORT-INPUT  SECTION.                                                   
018600                                                                          
018700     PERFORM S01-LAES-W51067                                              
018800     PERFORM UNTIL END-OF-W51067                                          
018900       MOVE IN-EKHT-IDDC-SEND      TO WS-IDDC                             
019000                                      W-IDDC-B6                           
019100       PERFORM IMS-GU-WDB601                                              
019200       IF (IN-EKHT-KDEKHHT = '201' OR                                     
019300           IN-EKHT-KDEKHHT = '202' OR                                     
019400           IN-EKHT-KDEKHHT = '203' OR                                     
019500           IN-EKHT-KDEKHHT = '204' OR                                     
019600           IN-EKHT-KDEKHHT = '404' OR                                     
019700           IN-EKHT-KDEKHHT = '501') AND                                   
019800          (SEGMENT-FINNS AND                                              
019900          (DCS-CDC OR DCS-CDC-TR OR DCS-SDC OR DCS-JAPAN OR               
020000          (DCS-AUSTRALIA)))                                               
020100         MOVE IN-AREA TO SORTWS-AREA                                      
020200         PERFORM S02-SORT-RELEASE                                         
020300       END-IF                                                             
020400       PERFORM S01-LAES-W51067                                            
020500     END-PERFORM                                                          
020600     .                                                                    
020700     EJECT                                                                
020800 C-SORT-OUTPUT SECTION.                                                   
020900                                                                          
021000     PERFORM S03-SORT-RETURN                                              
021100                                                                          
021200     PERFORM UNTIL SORTFIL-EOF-SW = JA                                    
021300                                                                          
021400       PERFORM CA-RAEKNA-UT-PRISER                                        
021500       IF SORT-EKHT-KDEKNIVA = 'DET'                                      
021600          PERFORM CB-BEHANDLA-942-POSTER                                  
021700          PERFORM S05-SKRIV-W54105                                        
021800       END-IF                                                             
021900       MOVE SPACE              TO UT-AREA                                 
022000                                                                          
022100       PERFORM S03-SORT-RETURN                                            
022200                                                                          
022300     END-PERFORM                                                          
022400     .                                                                    
022500     EJECT                                                                
022600 CA-RAEKNA-UT-PRISER SECTION.                                             
022700                                                                          
022800     COMPUTE W-SUARTSTD = SORT-EKHT-PRARTSTD * SORT-EKHT-KVANTAL          
022900     COMPUTE W-SUARTSJK = SORT-EKHT-PRARTSJK * SORT-EKHT-KVANTAL          
023000     COMPUTE W-SUARTNTO = SORT-EKHT-PRARTNTO * SORT-EKHT-KVANTAL          
023100     .                                                                    
023200     EJECT                                                                
023300 CB-BEHANDLA-942-POSTER SECTION.                                          
023400                                                                          
023500     MOVE SPACE             TO UT942-AREA                                 
023600     MOVE '942'             TO UT942-IDPTYP                               
023700     MOVE IN-EKHT-IDARTNR   TO UT942-IDARTNR                              
023800     MOVE IN-EKHT-IDDC-SEND TO UT942-IDDC                                 
023900     EVALUATE SORT-EKHT-KDEKHHT                                           
024000     WHEN '201'                                                           
024100        MOVE 'G'            TO UT942-KDFAKTYP                             
024200     WHEN '202'                                                           
024300        MOVE 'K'            TO UT942-KDFAKTYP                             
024400     WHEN '203'                                                           
024500        MOVE 'N'            TO UT942-KDFAKTYP                             
024600     WHEN '204'                                                           
024700        MOVE 'R'            TO UT942-KDFAKTYP                             
024800     WHEN '404'                                                           
024900        MOVE 'N'            TO UT942-KDFAKTYP                             
025000     WHEN '501'                                                           
025100        MOVE 'K'            TO UT942-KDFAKTYP                             
025200     END-EVALUATE                                                         
025300     MOVE IN-EKHT-KVANTAL   TO UT942-KVLEVART                             
025400     MOVE W-SUARTSTD        TO UT942-SUARTSTD                             
025500     MOVE IN-EKHT-IDDISTR   TO UT942-IDDISTR                              
025600     MOVE IN-EKHT-IDKUNDNR  TO UT942-IDKUNDNR                             
025700     MOVE W-SUARTSJK        TO UT942-SUARTSJK                             
025800     MOVE ZERO              TO UT942-SUARTSJK-V                           
025900                               UT942-IDLKTO                               
026000     MOVE W-SUARTNTO        TO UT942-SUARTNTO                             
026100     MOVE IN-EKHT-KDPRODSL  TO UT942-KDPRODSL                             
026200     MOVE IN-EKHT-IDVERGL   TO W-IDVERGL                                  
026300     PERFORM CZ-FIXA-IDFAKT                                               
026400     MOVE W-IDFAKT          TO UT942-IDFAKT                               
026500     MOVE IN-EKHT-FLLSBOK   TO UT942-FLLSBOK                              
026600     MOVE IN-EKHT-IDKONTO   TO UT942-IDKONTO                              
026700     MOVE IN-EKHT-IDKST     TO UT942-IDKST                                
026800     .                                                                    
026900     EJECT                                                                
027000 CZ-FIXA-IDFAKT SECTION.                                                  
027100                                                                          
027200     IF W-IDVERGL(10:1) NOT NUMERIC                                       
027300       IF W-IDVERGL(9:1) NOT NUMERIC                                      
027400         IF W-IDVERGL(8:1) NOT NUMERIC                                    
027500           IF W-IDVERGL(7:1) NOT NUMERIC                                  
027600             IF W-IDVERGL(6:1) NOT NUMERIC                                
027700               IF W-IDVERGL(5:1) NOT NUMERIC                              
027800                 IF W-IDVERGL(4:1) NOT NUMERIC                            
027900                   IF W-IDVERGL(3:1) NOT NUMERIC                          
028000                     IF W-IDVERGL(2:1) NOT NUMERIC                        
028100                       IF W-IDVERGL(1:1) NOT NUMERIC                      
028200                         MOVE ZERO         TO W-IDFAKT-X                  
028300                       END-IF                                             
028400                     ELSE                                                 
028500                       MOVE W-IDVERGL(1:2) TO W-IDFAKT-X                  
028600                     END-IF                                               
028700                   ELSE                                                   
028800                     MOVE W-IDVERGL(1:3)   TO W-IDFAKT-X                  
028900                   END-IF                                                 
029000                 ELSE                                                     
029100                   MOVE W-IDVERGL(1:4)     TO W-IDFAKT-X                  
029200                 END-IF                                                   
029300               ELSE                                                       
029400                 MOVE W-IDVERGL(1:5)       TO W-IDFAKT-X                  
029500               END-IF                                                     
029600             ELSE                                                         
029700               MOVE W-IDVERGL(1:6)         TO W-IDFAKT-X                  
029800             END-IF                                                       
029900           ELSE                                                           
030000             MOVE W-IDVERGL(1:7)           TO W-IDFAKT-X                  
030100           END-IF                                                         
030200         ELSE                                                             
030300           MOVE W-IDVERGL(2:7)             TO W-IDFAKT-X                  
030400         END-IF                                                           
030500       ELSE                                                               
030600         MOVE W-IDVERGL(3:7)               TO W-IDFAKT-X                  
030700       END-IF                                                             
030800     ELSE                                                                 
030900       MOVE W-IDVERGL(4:7)                 TO W-IDFAKT-X                  
031000     END-IF                                                               
031100     MOVE W-IDFAKT-X                       TO W-IDFAKT                    
031200     .                                                                    
031300     EJECT                                                                
031400 Z-FINIT SECTION.                                                         
031500                                                                          
031600     CLOSE W51067                                                         
031700           W54105                                                         
031800                                                                          
031900     MOVE 'S' TO POSTSUM-OPKOD                                            
032000     CALL POSTSUM USING POSTSUM-PARM                                      
032100     .                                                                    
032200     EJECT                                                                
032300 S01-LAES-W51067  SECTION.                                                
032400                                                                          
032500     READ W51067 INTO IN-AREA                                             
032600     AT END                                                               
032700        MOVE HIGH-VALUE   TO IN-AREA                                      
032800        SET END-OF-W51067 TO TRUE                                         
032900                                                                          
033000     NOT AT END                                                           
033100        MOVE 'W51067'     TO POSTSUM-FDNAMN                               
033200        MOVE 'W54105D1'   TO POSTSUM-DDNAMN2                              
033300        MOVE 'IN  '       TO POSTSUM-TRANSTYP                             
033400        CALL POSTSUM USING POSTSUM-PARM                                   
033500     END-READ                                                             
033600     .                                                                    
033700     EJECT                                                                
033800 S02-SORT-RELEASE  SECTION.                                               
033900                                                                          
034000     RELEASE SORT-POST FROM SORTWS-AREA                                   
034100     .                                                                    
034200     EJECT                                                                
034300 S03-SORT-RETURN  SECTION.                                                
034400                                                                          
034500     RETURN SORTFIL INTO IN-AREA                                          
034600     AT END                                                               
034700       MOVE JA            TO SORTFIL-EOF-SW                               
034800     END-RETURN                                                           
034900     .                                                                    
035000     EJECT                                                                
035100 S05-SKRIV-W54105 SECTION.                                                
035200                                                                          
035300     WRITE UTALL-942-POST FROM UT942-AREA                                 
035400                                                                          
035500     MOVE '942'           TO POSTSUM-TRANSTYP                             
035600     MOVE 'W54105'        TO POSTSUM-FDNAMN                               
035700     MOVE 'W54105D2'      TO POSTSUM-DDNAMN2                              
035800     CALL POSTSUM USING POSTSUM-PARM                                      
035900     .                                                                    
036000     EJECT                                                                
036100 S99-ABEND SECTION.                                                       
036200                                                                          
036300     SKIP2                                                                
036400     MOVE 'S' TO POSTSUM-OPKOD                                            
036500     CALL POSTSUM USING POSTSUM-PARM                                      
036600     CALL ABEND USING RKOD-ABEND                                          
036700     .                                                                    
036800     EJECT                                                                
036900* IMS SECTIONER                                                           
037000     SKIP2                                                                
037100 IMS-GU-WDB601    SECTION.                                                
037200     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
037300          DELIMITED BY SIZE INTO SSA1                                     
037400     MOVE '  GE' TO GODK-STATUSKODER                                      
037500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
037600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
037700     PERFORM IMS-STATUSKONTROLL                                           
037800     .                                                                    
037900     EJECT                                                                
038000 IMS-STATUSKONTROLL SECTION.                                              
038100                                                                          
038200     SET STATUS-IX TO 1                                                   
038300     SEARCH GODK-STATUS AT END CALL FELLOG                                
038400        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                          
038500        CONTINUE                                                          
038600     END-SEARCH                                                           
038700     .                                                                    
