000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W5602200.                                                
000400 AUTHOR.         INGVAR SKJELBRED.                                        
000500 DATE-WRITTEN.   96/12/13.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        COBOL SORT PROGRAM                                               
001100*        SORTERAR POSTER AV TYP A01 -A20                                  
001200*                                                                         
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
002501     SKIP2                                                                
002502*          --- LAB POSTER AV TYP A01 - A20                                
002503     SELECT W56010                     ASSIGN TO W56022D1.                
002504     SKIP2                                                                
002505*          --- POSTER TILL VCNA                                           
002506     SELECT W56022                     ASSIGN TO W56022D2.                
002507     SKIP2                                                                
002508*          --- POSTER TILL VCL                                            
002510     SELECT W56023                     ASSIGN TO W56022D3.                
002600     SKIP2                                                                
002700*          --- SORTERINGSFIL                                              
002800     SELECT SORTFIL                    ASSIGN TO W56022DS.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003401     SKIP3                                                                
003402 FD  W56010                                                               
003403     RECORDING       V                                                    
003404     BLOCK CONTAINS  0.                                                   
003405                                                                          
003406*01  -COPY W510A01      -L.                                               
003408*01  -COPY W510A02      -L.                                               
003410*01  -COPY W510A03      -L.                                               
003412*01  -COPY W510A04      -L.                                               
003414*01  -COPY W510A05      -L.                                               
003416*01  -COPY W510A06      -L.                                               
003418*01  -COPY W510A07      -L.                                               
003420*01  -COPY W510A08      -L.                                               
003422*01  -COPY W510A09      -L.                                               
003424*01  -COPY W510A10      -L.                                               
003426*01  -COPY W510A11      -L.                                               
003428*01  -COPY W510A12      -L.                                               
003430*01  -COPY W510A13      -L.                                               
003432*01  -COPY W510A14      -L.                                               
003434*01  -COPY W510A15      -L.                                               
003436*01  -COPY W510A16      -L.                                               
003438*01  -COPY W510A17      -L.                                               
003440*01  -COPY W510A18      -L.                                               
003442*01  -COPY W510A19      -L.                                               
003444*01  -COPY W510A20      -L.                                               
003445     SKIP3                                                                
003446 FD  W56022                                                               
003447     RECORDING       V                                                    
003448     BLOCK CONTAINS  0.                                                   
003449                                                                          
003450*01  POST -COPY W510A01 -PRE  1A01-  -L.                                  
003451*01  POST -COPY W510A02 -PRE  1A02-  -L.                                  
003452*01  POST -COPY W510A03 -PRE  1A03-  -L.                                  
003453*01  POST -COPY W510A04 -PRE  1A04-  -L.                                  
003454*01  POST -COPY W510A05 -PRE  1A05-  -L.                                  
003455*01  POST -COPY W510A06 -PRE  1A06-  -L.                                  
003456*01  POST -COPY W510A07 -PRE  1A07-  -L.                                  
003457*01  POST -COPY W510A08 -PRE  1A08-  -L.                                  
003458*01  POST -COPY W510A09 -PRE  1A09-  -L.                                  
003459*01  POST -COPY W510A10 -PRE  1A10-  -L.                                  
003460*01  POST -COPY W510A11 -PRE  1A11-  -L.                                  
003461*01  POST -COPY W510A12 -PRE  1A12-  -L.                                  
003462*01  POST -COPY W510A13 -PRE  1A13-  -L.                                  
003463*01  POST -COPY W510A14 -PRE  1A14-  -L.                                  
003464*01  POST -COPY W510A15 -PRE  1A15-  -L.                                  
003465*01  POST -COPY W510A16 -PRE  1A16-  -L.                                  
003466*01  POST -COPY W510A17 -PRE  1A17-  -L.                                  
003467*01  POST -COPY W510A18 -PRE  1A18-  -L.                                  
003468*01  POST -COPY W510A19 -PRE  1A19-  -L.                                  
003470*01  POST -COPY W510A20 -PRE  1A20-  -L.                                  
003471     SKIP3                                                                
003472 FD  W56023                                                               
003473     RECORDING       V                                                    
003474     BLOCK CONTAINS  0.                                                   
003475                                                                          
003476*01  POST -COPY W510A01 -PRE  2A01-  -L.                                  
003477*01  POST -COPY W510A02 -PRE  2A02-  -L.                                  
003478*01  POST -COPY W510A03 -PRE  2A03-  -L.                                  
003479*01  POST -COPY W510A04 -PRE  2A04-  -L.                                  
003480*01  POST -COPY W510A05 -PRE  2A05-  -L.                                  
003481*01  POST -COPY W510A06 -PRE  2A06-  -L.                                  
003482*01  POST -COPY W510A07 -PRE  2A07-  -L.                                  
003483*01  POST -COPY W510A08 -PRE  2A08-  -L.                                  
003484*01  POST -COPY W510A09 -PRE  2A09-  -L.                                  
003485*01  POST -COPY W510A10 -PRE  2A10-  -L.                                  
003486*01  POST -COPY W510A11 -PRE  2A11-  -L.                                  
003487*01  POST -COPY W510A12 -PRE  2A12-  -L.                                  
003488*01  POST -COPY W510A13 -PRE  2A13-  -L.                                  
003489*01  POST -COPY W510A14 -PRE  2A14-  -L.                                  
003490*01  POST -COPY W510A15 -PRE  2A15-  -L.                                  
003491*01  POST -COPY W510A16 -PRE  2A16-  -L.                                  
003492*01  POST -COPY W510A17 -PRE  2A17-  -L.                                  
003493*01  POST -COPY W510A18 -PRE  2A18-  -L.                                  
003494*01  POST -COPY W510A19 -PRE  2A19-  -L.                                  
003496*01  POST -COPY W510A20 -PRE  2A20-  -L.                                  
003599     SKIP2                                                                
003600 SD  SORTFIL.                                                             
003701                                                                          
003702 01  SORT-AREA.                                                           
003703     04  SORT-TYP            PIC X.                                       
003705     04  SORT-IDFAKTNR       PIC 9(7).                                    
003706     04  SORT-IDPTYP         PIC X(3).                                    
003707     04  SORT-POST           PIC X(200).                                  
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004001                                                                          
004010*    -- CHECKED BY WY2000                                                 
004100 77  IDPGM                       PIC X(8)    VALUE 'W5602200'.            
004200 77  JA                          PIC X       VALUE 'J'.                   
004300 77  NEJ                         PIC X       VALUE 'N'.                   
004501                                                                          
004502 77  W56010-EOF-SW               PIC X       VALUE 'N'.                   
004510     88  END-OF-W56010                       VALUE 'J'.                   
004600                                                                          
004700 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
004800     88  END-OF-SORTFIL                      VALUE 'J'.                   
004900     EJECT                                                                
005000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005100 01  FILLER REDEFINES DAGENS-DATUM.                                       
005200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005500     EJECT                                                                
005600 01  DYNAMISKA-SUBPROGRAM.                                                
005700*                                                                         
005800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005900     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
006001     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006010     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006100     SKIP2                                                                
006200*    --- PARAMETRAR TILL ABEND                                            
006300                                                                          
006400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006600     SKIP2                                                                
006700 01  FELTEXT.                                                             
006800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007000     EJECT                                                                
007100*    --- PARAMETRAR TILL DATKORT                                          
007200*                                                                         
007300 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W56022'.              
007400     SKIP2                                                                
007500 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
007600     SKIP2                                                                
007700*01  -COPY WDATKORT                                                       
007801     EJECT                                                                
007802*    --- PARAMETRAR TILL POSTSUM                                          
007803*                                                                         
007810*01  -COPY W0005   -PRE  POSTSUM-                                         
007901     EJECT                                                                
007910*01  -COPY WDATAREA                                                       
008001     EJECT                                                                
008002 01  IN-AREA-START             PIC X(24)   VALUE                          
008003                                 'IN-AREA-START  '.                       
008004     SKIP2                                                                
008005 01  IN-AREA.                                                             
008006     03  IN-AREA-0.                                                       
008007       05  IN-IDPTYP           PIC X(3).                                  
008009       05  FILLER                PIC X(200).                              
008010*   03  FILLER -COPY W510A01  -PRE IA01-  -RED  IN-AREA-0                 
008011*   03  FILLER -COPY W510A02  -PRE IA02-  -RED  IN-AREA-0                 
008012*   03  FILLER -COPY W510A03  -PRE IA03-  -RED  IN-AREA-0                 
008015*   03  FILLER -COPY W510A04  -PRE IA04-  -RED  IN-AREA-0                 
008017*   03  FILLER -COPY W510A05  -PRE IA05-  -RED  IN-AREA-0                 
008019*   03  FILLER -COPY W510A06  -PRE IA06-  -RED  IN-AREA-0                 
008021*   03  FILLER -COPY W510A07  -PRE IA07-  -RED  IN-AREA-0                 
008023*   03  FILLER -COPY W510A08  -PRE IA08-  -RED  IN-AREA-0                 
008025*   03  FILLER -COPY W510A09  -PRE IA09-  -RED  IN-AREA-0                 
008027*   03  FILLER -COPY W510A10  -PRE IA10-  -RED  IN-AREA-0                 
008029*   03  FILLER -COPY W510A11  -PRE IA11-  -RED  IN-AREA-0                 
008031*   03  FILLER -COPY W510A12  -PRE IA12-  -RED  IN-AREA-0                 
008033*   03  FILLER -COPY W510A13  -PRE IA13-  -RED  IN-AREA-0                 
008035*   03  FILLER -COPY W510A14  -PRE IA14-  -RED  IN-AREA-0                 
008037*   03  FILLER -COPY W510A15  -PRE IA15-  -RED  IN-AREA-0                 
008039*   03  FILLER -COPY W510A16  -PRE IA16-  -RED  IN-AREA-0                 
008041*   03  FILLER -COPY W510A17  -PRE IA17-  -RED  IN-AREA-0                 
008043*   03  FILLER -COPY W510A18  -PRE IA18-  -RED  IN-AREA-0                 
008045*   03  FILLER -COPY W510A19  -PRE IA19-  -RED  IN-AREA-0                 
008046*   03  FILLER -COPY W510A20  -PRE IA20-  -RED  IN-AREA-0                 
008047     EJECT                                                                
008050                                                                          
008059                                                                          
008061     EJECT                                                                
008062 01  UT-AREA-START             PIC X(24)   VALUE                          
008063                                 'UT-AREA-START  '.                       
008064     SKIP2                                                                
008065 01  UT-AREA.                                                             
008066     03  UT-AREA-0.                                                       
008067       05  UT-IDPTYP             PIC X(3).                                
008068       05  FILLER                PIC X(3).                                
008069       05  UT-IDFTG              PIC 9(2).                                
008070       05  FILLER                PIC X(200).                              
008071*   03  FILLER -COPY W510A01  -PRE UA01-  -RED  UT-AREA-0                 
008072*   03  FILLER -COPY W510A02  -PRE UA02-  -RED  UT-AREA-0                 
008073*   03  FILLER -COPY W510A03  -PRE UA03-  -RED  UT-AREA-0                 
008074*   03  FILLER -COPY W510A04  -PRE UA04-  -RED  UT-AREA-0                 
008075*   03  FILLER -COPY W510A05  -PRE UA05-  -RED  UT-AREA-0                 
008076*   03  FILLER -COPY W510A06  -PRE UA06-  -RED  UT-AREA-0                 
008077*   03  FILLER -COPY W510A07  -PRE UA07-  -RED  UT-AREA-0                 
008078*   03  FILLER -COPY W510A08  -PRE UA08-  -RED  UT-AREA-0                 
008079*   03  FILLER -COPY W510A09  -PRE UA09-  -RED  UT-AREA-0                 
008080*   03  FILLER -COPY W510A10  -PRE UA10-  -RED  UT-AREA-0                 
008081*   03  FILLER -COPY W510A11  -PRE UA11-  -RED  UT-AREA-0                 
008082*   03  FILLER -COPY W510A12  -PRE UA12-  -RED  UT-AREA-0                 
008083*   03  FILLER -COPY W510A13  -PRE UA13-  -RED  UT-AREA-0                 
008084*   03  FILLER -COPY W510A14  -PRE UA14-  -RED  UT-AREA-0                 
008085*   03  FILLER -COPY W510A15  -PRE UA15-  -RED  UT-AREA-0                 
008086*   03  FILLER -COPY W510A16  -PRE UA16-  -RED  UT-AREA-0                 
008087*   03  FILLER -COPY W510A17  -PRE UA17-  -RED  UT-AREA-0                 
008088*   03  FILLER -COPY W510A18  -PRE UA18-  -RED  UT-AREA-0                 
008089*   03  FILLER -COPY W510A19  -PRE UA19-  -RED  UT-AREA-0                 
008090*   03  FILLER -COPY W510A20  -PRE UA20-  -RED  UT-AREA-0                 
008200 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
008300     EJECT                                                                
008400 PROCEDURE DIVISION.                                                      
008500 MAIN SECTION.                                                            
008700     SKIP2                                                                
008800                                                                          
008900     PERFORM A-INIT                                                       
009000                                                                          
009100     SORT SORTFIL ASCENDING SORT-TYP                                      
009200                            SORT-IDFAKTNR                                 
009210                            SORT-IDPTYP                                   
009300                  INPUT PROCEDURE B-SORT-INPUT                            
009400                  OUTPUT PROCEDURE C-SORT-OUTPUT                          
009500                                                                          
009600     IF SORT-RETURN NOT = 0                                               
009700       MOVE SORT-RETURN TO SORT-RETURN-X                                  
009800       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
009900       DELIMITED BY SIZE INTO FELTEXT-STR                                 
010000       DISPLAY FELTEXT                                                    
010100       PERFORM S99-ABEND                                                  
010200     ELSE                                                                 
010300       PERFORM Z-FINIT                                                    
010400                                                                          
010500       MOVE ZERO TO RETURN-CODE                                           
010600       GOBACK                                                             
010700     END-IF                                                               
010800                                                                          
010900     .                                                                    
011000     EJECT                                                                
011100 A-INIT SECTION.                                                          
011201                                                                          
011210     OPEN INPUT  W56010                                                   
011301                                                                          
011302     OPEN OUTPUT W56022                                                   
011310                 W56023                                                   
011400     SKIP2                                                                
011500     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
011600     MOVE D-AAR     TO  DAGENS-DATUM-AAR                                  
011700     MOVE D-MAANAD  TO  DAGENS-DATUM-MAANAD                               
011800     MOVE D-DAG     TO  DAGENS-DATUM-DAG                                  
011910     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
012000     .                                                                    
012101     EJECT                                                                
012102 B-SORT-INPUT  SECTION.                                                   
012104                                                                          
012105     PERFORM S01-LAES-W56010                                              
012106     PERFORM UNTIL END-OF-W56010                                          
012108                                                                          
012109         MOVE IN-AREA      TO SORT-POST                                   
012112         EVALUATE IN-IDPTYP                                               
012113            WHEN 'A01'                                                    
012114               MOVE 'A'    TO SORT-TYP                                    
012115               MOVE IA01-IDFAKT TO SORT-IDFAKTNR                          
012116               MOVE IN-IDPTYP TO SORT-IDPTYP                              
012117            WHEN 'A02'                                                    
012118               MOVE 'A'    TO SORT-TYP                                    
012119               MOVE IA02-IDFAKT TO SORT-IDFAKTNR                          
012120               MOVE IN-IDPTYP TO SORT-IDPTYP                              
012121            WHEN 'A03'                                                    
012122               MOVE 'B'    TO SORT-TYP                                    
012123               MOVE IA03-IDFAKT TO SORT-IDFAKTNR                          
012124               MOVE IN-IDPTYP TO SORT-IDPTYP                              
012125            WHEN 'A04'                                                    
012126               MOVE 'C'    TO SORT-TYP                                    
012127               MOVE IA04-IDKNOTNR TO SORT-IDFAKTNR                        
012128               MOVE IN-IDPTYP TO SORT-IDPTYP                              
012129            WHEN 'A05'                                                    
012130               MOVE 'C'    TO SORT-TYP                                    
012131               MOVE IA05-IDKNOTNR TO SORT-IDFAKTNR                        
012132               MOVE IN-IDPTYP TO SORT-IDPTYP                              
012133            WHEN OTHER                                                    
012134               MOVE 'D'    TO SORT-TYP                                    
012135               MOVE ZERO   TO SORT-IDFAKTNR                               
012136               MOVE IN-IDPTYP TO SORT-IDPTYP                              
012137         END-EVALUATE                                                     
012138         PERFORM S31-SORT-RELEASE                                         
012139       PERFORM S01-LAES-W56010                                            
012140     END-PERFORM                                                          
012150     .                                                                    
012200     EJECT                                                                
012300 C-SORT-OUTPUT SECTION.                                                   
012400     SKIP2                                                                
012500     PERFORM S32-SORT-RETURN                                              
012600     PERFORM UNTIL END-OF-SORTFIL                                         
012800       PERFORM CA-BEHANDLA                                                
012900       PERFORM S32-SORT-RETURN                                            
013000     END-PERFORM                                                          
013100     .                                                                    
013200     EJECT                                                                
013210 CA-BEHANDLA SECTION.                                                     
013213     IF UT-IDFTG = 53                                                     
013214        PERFORM S11-SKRIV-W56022                                          
013215     END-IF                                                               
013216     IF UT-IDFTG = 54                                                     
013217        PERFORM S12-SKRIV-W56023                                          
013218     END-IF                                                               
013219                                                                          
013261     .                                                                    
013270     EJECT                                                                
013300 Z-FINIT SECTION.                                                         
013401     CLOSE W56010                                                         
013402           W56022                                                         
013410           W56023                                                         
013501     SKIP2                                                                
013502     MOVE 'S' TO POSTSUM-OPKOD                                            
013510     CALL POSTSUM USING POSTSUM-PARM                                      
013600     .                                                                    
013701     EJECT                                                                
013702 S01-LAES-W56010  SECTION.                                                
013704     READ W56010 INTO IN-AREA                                             
013705     AT END                                                               
013706        SET END-OF-W56010 TO TRUE                                         
013707                                                                          
013708     NOT AT END                                                           
013709        MOVE 'W56010' TO POSTSUM-FDNAMN                                   
013710        MOVE 'W56022D1' TO POSTSUM-DDNAMN2                                
013711        MOVE IN-IDPTYP TO POSTSUM-TRANSTYP                                
013712        CALL POSTSUM USING POSTSUM-PARM                                   
013713     END-READ                                                             
013720     .                                                                    
013801     EJECT                                                                
013802 S11-SKRIV-W56022 SECTION.                                                
013805     EVALUATE UT-IDPTYP                                                   
013806       WHEN 'A01'                                                         
013807            WRITE 1A01-POST FROM UT-AREA                                  
013808       WHEN 'A02'                                                         
013809            WRITE 1A02-POST FROM UT-AREA                                  
013810       WHEN 'A03'                                                         
013811            WRITE 1A03-POST FROM UT-AREA                                  
013812       WHEN 'A04'                                                         
013813            WRITE 1A04-POST FROM UT-AREA                                  
013814       WHEN 'A05'                                                         
013815            WRITE 1A05-POST FROM UT-AREA                                  
013816       WHEN 'A06'                                                         
013817            WRITE 1A06-POST FROM UT-AREA                                  
013818       WHEN 'A07'                                                         
013819            WRITE 1A07-POST FROM UT-AREA                                  
013820       WHEN 'A08'                                                         
013821            WRITE 1A08-POST FROM UT-AREA                                  
013822       WHEN 'A09'                                                         
013823            WRITE 1A09-POST FROM UT-AREA                                  
013824       WHEN 'A10'                                                         
013825            WRITE 1A10-POST FROM UT-AREA                                  
013826       WHEN 'A11'                                                         
013827            WRITE 1A11-POST FROM UT-AREA                                  
013828       WHEN 'A12'                                                         
013829            WRITE 1A12-POST FROM UT-AREA                                  
013830       WHEN 'A13'                                                         
013831            WRITE 1A13-POST FROM UT-AREA                                  
013832       WHEN 'A14'                                                         
013833            WRITE 1A14-POST FROM UT-AREA                                  
013834       WHEN 'A15'                                                         
013835            WRITE 1A15-POST FROM UT-AREA                                  
013836       WHEN 'A16'                                                         
013837            WRITE 1A16-POST FROM UT-AREA                                  
013838       WHEN 'A17'                                                         
013839            WRITE 1A17-POST FROM UT-AREA                                  
013840       WHEN 'A18'                                                         
013841            WRITE 1A18-POST FROM UT-AREA                                  
013842       WHEN 'A19'                                                         
013843            WRITE 1A19-POST FROM UT-AREA                                  
013844       WHEN 'A20'                                                         
013845            WRITE 1A20-POST FROM UT-AREA                                  
013846     END-EVALUATE                                                         
013847                                                                          
013848     MOVE UT-IDPTYP TO POSTSUM-TRANSTYP                                   
013849     MOVE 'W56022' TO POSTSUM-FDNAMN                                      
013850     MOVE 'W56022D2' TO POSTSUM-DDNAMN2                                   
013851     CALL POSTSUM USING POSTSUM-PARM                                      
013852     .                                                                    
013853     EJECT                                                                
013854 S12-SKRIV-W56023 SECTION.                                                
013857                                                                          
013858     EVALUATE UT-IDPTYP                                                   
013859       WHEN 'A01'                                                         
013860            WRITE 2A01-POST FROM UT-AREA                                  
013861       WHEN 'A02'                                                         
013862            WRITE 2A02-POST FROM UT-AREA                                  
013863       WHEN 'A03'                                                         
013864            WRITE 2A03-POST FROM UT-AREA                                  
013865       WHEN 'A04'                                                         
013866            WRITE 2A04-POST FROM UT-AREA                                  
013867       WHEN 'A05'                                                         
013868            WRITE 2A05-POST FROM UT-AREA                                  
013869       WHEN 'A06'                                                         
013870            WRITE 2A06-POST FROM UT-AREA                                  
013871       WHEN 'A07'                                                         
013872            WRITE 2A07-POST FROM UT-AREA                                  
013873       WHEN 'A08'                                                         
013874            WRITE 2A08-POST FROM UT-AREA                                  
013875       WHEN 'A09'                                                         
013876            WRITE 2A09-POST FROM UT-AREA                                  
013877       WHEN 'A10'                                                         
013878            WRITE 2A10-POST FROM UT-AREA                                  
013879       WHEN 'A11'                                                         
013880            WRITE 2A11-POST FROM UT-AREA                                  
013881       WHEN 'A12'                                                         
013882            WRITE 2A12-POST FROM UT-AREA                                  
013883       WHEN 'A13'                                                         
013884            WRITE 2A13-POST FROM UT-AREA                                  
013885       WHEN 'A14'                                                         
013886            WRITE 2A14-POST FROM UT-AREA                                  
013887       WHEN 'A15'                                                         
013888            WRITE 2A15-POST FROM UT-AREA                                  
013889       WHEN 'A16'                                                         
013890            WRITE 2A16-POST FROM UT-AREA                                  
013891       WHEN 'A17'                                                         
013892            WRITE 2A17-POST FROM UT-AREA                                  
013893       WHEN 'A18'                                                         
013894            WRITE 2A18-POST FROM UT-AREA                                  
013895       WHEN 'A19'                                                         
013896            WRITE 2A19-POST FROM UT-AREA                                  
013897       WHEN 'A20'                                                         
013898            WRITE 2A20-POST FROM UT-AREA                                  
013899     END-EVALUATE                                                         
013900                                                                          
013901     MOVE UT-IDPTYP TO POSTSUM-TRANSTYP                                   
013902     MOVE 'W56023' TO POSTSUM-FDNAMN                                      
013903     MOVE 'W56022D3' TO POSTSUM-DDNAMN2                                   
013904     CALL POSTSUM USING POSTSUM-PARM                                      
013910     .                                                                    
014000     EJECT                                                                
014100 S31-SORT-RELEASE  SECTION.                                               
014200                                                                          
014300     RELEASE SORT-AREA                                                    
014400     .                                                                    
014500     EJECT                                                                
014600 S32-SORT-RETURN  SECTION.                                                
014700                                                                          
014800     RETURN SORTFIL                                                       
014900     AT END                                                               
015000         SET END-OF-SORTFIL TO TRUE                                       
015010     NOT AT END                                                           
015011       MOVE SORT-POST      TO UT-AREA                                     
015020     END-RETURN                                                           
015100     .                                                                    
015200     EJECT                                                                
015300 S99-ABEND SECTION.                                                       
015400                                                                          
015501     SKIP2                                                                
015502     MOVE 'S' TO POSTSUM-OPKOD                                            
015510     CALL POSTSUM USING POSTSUM-PARM                                      
015600     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
015700     .                                                                    
