001000 ID DIVISION.                                                             
001100     SKIP2                                                                
001200 PROGRAM-ID.     W4267100.                                                
001300*AUTHOR.         ANN WESTBERG.                                            
001400*DATE-WRITTEN.   92/10/12.                                                
001500                                                                          
001600*    REMARKS.                                                             
001700*                                                                         
001800*    FUNKTION:                                                            
001900*        SB FÖR ATT SKRIVA EN UTFIL (W42671)                              
002000*        MED KVALITETSINFO FÖR E+ PROGRAM                                 
002010*                                                                         
002020*        TILLÄGG 990101: SKAPAR EN EXTRA UTFIL MED R-NOT ARTIKLAR         
002030*        > 1 ÅR FÖR SENARE UTSKRIFT AV LISTA.                             
002040*                                                                         
002100*                                                                         
002210*        PROGRAMMET LÄSER      W6D2                                       
002300*                                                                         
002400*    ABENDKODER:                                                          
002500*        U0016 -  . . . .                                                 
002600*        U1000 -  . . . .                                                 
002700*                                                                         
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     SKIP2                                                                
003200 INPUT-OUTPUT SECTION.                                                    
003300                                                                          
003400 FILE-CONTROL.                                                            
003501     SKIP2                                                                
003502*          --- KVALITETSINFO (W6D211) FÖR E+ PROGRAM                      
003510     SELECT W42671                     ASSIGN TO W42671D1.                
003520     SELECT W4267A                     ASSIGN TO W42671D2.                
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP3                                                                
004000 FILE SECTION.                                                            
004101     SKIP3                                                                
004102 FD  W42671                                                               
004103     RECORDING       F                                                    
004104     BLOCK CONTAINS  0.                                                   
004105     SKIP2                                                                
004110*01  POST -COPY W4267101 -PRE  UT-  -L.                                   
004200     EJECT                                                                
004210 FD  W4267A                                                               
004220     RECORDING       F                                                    
004230     BLOCK CONTAINS  0.                                                   
004240     SKIP2                                                                
004250*01  POST -COPY W4267102 -PRE  UT2-  -L.                                  
004260     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400     SKIP2                                                                
004401*    -- CHECKED BY WY2000                                                 
004410     SKIP3                                                                
004500 77  IDPGM                       PIC X(8)    VALUE 'W4267100'.            
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800 77  INDX                        PIC S9(4)   VALUE +0 COMP SYNC.          
004900 77  MAX-INDX                    PIC S9(4)   VALUE +7 COMP SYNC.          
005000     EJECT                                                                
005010 01  W1-DAREGDAT                 PIC 9(8).                                
005510 01  DAGENS-DATUM                PIC 9(8)    VALUE ZERO.                  
005520 01  WS-KVDAGAR                  PIC 9(4).                                
005600     EJECT                                                                
005700 01  DYNAMISKA-SUBPROGRAM.                                                
005800*                                                                         
005900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006210     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006300     SKIP2                                                                
006400*    --- PARAMETRAR TILL ABEND                                            
006500                                                                          
006600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006800     SKIP2                                                                
006900 01  FELTEXT.                                                             
007000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007201     EJECT                                                                
007202*    --- PARAMETRAR TILL POSTSUM                                          
007203*                                                                         
007210*01  -COPY W0005   -PRE  POSTSUM-                                         
007401     EJECT                                                                
007402                                                                          
007403 01  W426-AREA-START             PIC X(24)   VALUE                        
007404                                 'W426-AREA-START  '.                     
007405     SKIP2                                                                
007406                                                                          
007410*01  AREA -COPY W4267101     -PRE UT-                                     
007500     EJECT                                                                
007510 01  W426-AREA-START2            PIC X(24)   VALUE                        
007520                                 'W426-AREA-START2 '.                     
007530     SKIP2                                                                
007540                                                                          
007550*01  AREA -COPY W4267102     -PRE UT2-                                    
007560     EJECT                                                                
007600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007700*                                                                         
007800     EJECT                                                                
007900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008000     SKIP3                                                                
008010*    --- STATUS-KOD FRÅN IMS                                              
008020 01  STATUS-WS                   PIC XX.                                  
008030     88  SEGMENT-FINNS                       VALUE '  '.                  
008040     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
008050     SKIP2                                                                
008060 01  GODK-STATUSKODER.                                                    
008070     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
008080     SKIP3                                                                
008090 01  SSA1                        PIC X(64).                               
008091 01  SSA2                        PIC X(64).                               
008092     EJECT                                                                
008093                                                                          
008094*    --- NYCKLAR OCH SÖKFÄLT TILL DLI                                     
008095 01  FILLER                  PIC X(16) VALUE 'NYCKLAR-TILL-DLI'.          
008100 01  NYCKLAR-TILL-DLI.                                                    
008200                                                                          
008201     03  W-IDARTNR-X.                                                     
008202         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008203     03  W-W6D211KY-X.                                                    
008210         05  W-DAREGDAT-9KOMPL   PIC 9(8)    VALUE ZERO.                  
008220         05  W-TIKLOCK-9KOMPL    PIC S9(9)   VALUE ZERO COMP-3.           
008300     EJECT                                                                
008400                                                                          
009500*    --- IMS FUNKTIONSKODER                                               
009600*01  -COPY W0003                                                          
009700     EJECT                                                                
009900*    ---  DLI INPUT-OUTPUT AREA                                           
010000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010100     SKIP3                                                                
010200 01  DLI-IO-AREA.                                                         
010300     03  IO-AREA                 PIC X(1154)  VALUE SPACE.                
010401     SKIP3                                                                
010405     03  W6D211 REDEFINES IO-AREA.                                        
010410*        05  -COPY W6D211                                                 
010700     EJECT                                                                
010800 LINKAGE SECTION.                                                         
010900                                                                          
011020*01  -COPY W0008  -PRE W6D2-                                              
011030     05  KEYFB-IDARTNR           PIC S9(9) COMP-3.                        
011100     EJECT                                                                
011201 PROCEDURE DIVISION  USING W6D2-PCB.                                      
011210     ENTRY 'DLITCBL' USING W6D2-PCB.                                      
011300                                                                          
011500     SKIP2                                                                
011600     PERFORM A-INIT                                                       
011610                                                                          
011700     PERFORM B-BEHANDLA-INDATA                                            
011710                                                                          
011720     PERFORM Z-FINIT                                                      
011730                                                                          
011740     MOVE ZERO TO RETURN-CODE                                             
011750     GOBACK                                                               
011760     .                                                                    
011770     EJECT                                                                
013000                                                                          
013500 A-INIT SECTION.                                                          
013600                                                                          
013710     OPEN OUTPUT W42671                                                   
013720                 W4267A                                                   
014000     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
014010     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014200     .                                                                    
014300     EJECT                                                                
014301 B-BEHANDLA-INDATA SECTION.                                               
014302                                                                          
014310     PERFORM IMS-GN-W6D2                                                  
014312     PERFORM UNTIL SEGMENT-SAKNAS                                         
014313       EVALUATE W6D2-SEG-NAME-FB                                          
014314         WHEN  'W6D201'                                                   
014315           CONTINUE                                                       
014316         WHEN  'W6D211'                                                   
014319           PERFORM BA-BERAEKNA-REGDAT                                     
014320           MOVE KEYFB-IDARTNR TO UT-IDARTNR                               
014321                                 UT2-IDARTNR                              
014322           PERFORM S11-SKRIV-W42671                                       
014323           PERFORM C-KOLLA-RNOT-DATUM                                     
014324       END-EVALUATE                                                       
014330       PERFORM IMS-GN-W6D2                                                
014395     END-PERFORM                                                          
014407     .                                                                    
014408     EJECT                                                                
014419 BA-BERAEKNA-REGDAT SECTION.                                              
014420                                                                          
014431     COMPUTE W1-DAREGDAT = 99999999 -                                     
014432                               INFO-DAREGDAT-9KOMPL                       
014434     MOVE W1-DAREGDAT (3:6)       TO UT-TIREGDAT                          
014457     .                                                                    
014458     EJECT                                                                
014459 C-KOLLA-RNOT-DATUM SECTION.                                              
014460                                                                          
014461     IF INFO-KDKVAINF = 'R'                                               
014468       COMPUTE WS-KVDAGAR = DAGENS-DATUM - W1-DAREGDAT                    
014469       IF WS-KVDAGAR > 365                                                
014470         PERFORM S12-SKRIV-W4267A                                         
014471       END-IF                                                             
014472     END-IF                                                               
014473     .                                                                    
014474     EJECT                                                                
014475 S11-SKRIV-W42671 SECTION.                                                
014476                                                                          
014477     MOVE INFO-KDPERSON       TO UT-KDPERSON                              
014478     MOVE INFO-KDKVAINF       TO UT-KDKVAINF                              
014479                                                                          
014480     MOVE +1 TO INDX                                                      
014481     PERFORM UNTIL INDX > MAX-INDX                                        
014482       MOVE INFO-TEKVAINF-INT(INDX) TO UT-TEKVAINF-INT(INDX)              
014483       ADD +1 TO INDX                                                     
014484     END-PERFORM                                                          
014485                                                                          
014486     WRITE UT-POST FROM UT-AREA                                           
014487     MOVE 'W42671' TO POSTSUM-FDNAMN                                      
014488     MOVE 'W42671D1' TO POSTSUM-DDNAMN2                                   
014489     CALL POSTSUM USING POSTSUM-PARM                                      
014490     .                                                                    
014491     EJECT                                                                
014492 S12-SKRIV-W4267A SECTION.                                                
014493                                                                          
014494     MOVE INFO-KDKVAINF           TO UT2-KDKVAINF                         
014495     MOVE W1-DAREGDAT(3:6)        TO UT2-TIREGDAT                         
014496                                                                          
014497     WRITE UT2-POST FROM UT2-AREA                                         
014498     MOVE 'W4267A' TO POSTSUM-FDNAMN                                      
014499     MOVE 'W42671D2' TO POSTSUM-DDNAMN2                                   
014500     CALL POSTSUM USING POSTSUM-PARM                                      
014501     .                                                                    
014502     EJECT                                                                
014503 Z-FINIT SECTION.                                                         
014504                                                                          
014510     CLOSE W42671                                                         
014520           W4267A                                                         
014602     MOVE 'S' TO POSTSUM-OPKOD                                            
014610     CALL POSTSUM USING POSTSUM-PARM                                      
014700     .                                                                    
014901     EJECT                                                                
015800* --- IMS SEKTIONER ---                                                   
015900     SKIP3                                                                
016020 IMS-GN-W6D2 SECTION.                                                     
016025     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
016026     CALL CBLTDLI USING GN W6D2-PCB DLI-IO-AREA                           
016027     MOVE W6D2-STATUS-CODE TO STATUS-WS                                   
016028     PERFORM IMS-STATUSKONTROLL                                           
016029     .                                                                    
016030     SKIP3                                                                
016200 IMS-STATUSKONTROLL SECTION.                                              
016300     SKIP2                                                                
016400     SET STATUS-IX TO 1                                                   
016500     SEARCH GODK-STATUS                                                   
016600       AT END                                                             
016610         STRING 'FELAKTIG STATUSKOD FRÅN IMS:  ' STATUS-WS                
016620         DELIMITED BY SIZE INTO FELTEXT                                   
016900         CALL FELLOG                                                      
017000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
017100         CONTINUE                                                         
017200     END-SEARCH                                                           
017300     .                                                                    
