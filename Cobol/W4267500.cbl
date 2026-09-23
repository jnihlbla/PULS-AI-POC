001000 ID DIVISION.                                                             
001100     SKIP2                                                                
001200 PROGRAM-ID.     W4267500.                                                
001300*AUTHOR.         ANNELIE.                                                 
001400*DATE-WRITTEN.   93/12/10.                                                
001500                                                                          
001600*    REMARKS.                                                             
001700*                                                                         
001800*    FUNKTION:                                                            
001900*        SB FÖR ATT SKRIVA UTFIL (W42675,W42676,W42677)                   
002000*        MED KVALITETSINFO FÖR E+ PROGRAM                                 
002100*                                                                         
002210*        PROGRAMMET LÄSER      W6KVAH (W6D2)                              
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
003502*          --- KVALITETSINFO (W6D201) FÖR E+ PROGRAM                      
003510     SELECT W42675                     ASSIGN TO W42675D1.                
003520     SELECT W42676                     ASSIGN TO W42675D2.                
003530     SELECT W42677                     ASSIGN TO W42675D3.                
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP3                                                                
004000 FILE SECTION.                                                            
004101     SKIP3                                                                
004102 FD  W42675                                                               
004103     RECORDING       F                                                    
004104     BLOCK CONTAINS  0.                                                   
004105     SKIP2                                                                
004110*01  POST -COPY W4267501 -PRE  UT-  -L.                                   
004200     EJECT                                                                
004210 FD  W42676                                                               
004220     RECORDING       F                                                    
004230     BLOCK CONTAINS  0.                                                   
004240     SKIP2                                                                
004250*01  POST -COPY W4267501 -PRE  UT2-  -L.                                  
004260     EJECT                                                                
004270 FD  W42677                                                               
004280     RECORDING       F                                                    
004290     BLOCK CONTAINS  0.                                                   
004291     SKIP2                                                                
004292*01  POST -COPY W4267701 -PRE  UT3-  -L.                                  
004293     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400     SKIP2                                                                
004401                                                                          
004410*    -- CHECKED BY WY2000                                                 
004500 77  IDPGM                       PIC X(8)    VALUE 'W4267500'.            
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800 77  INDX                        PIC S9(4)   VALUE +0 COMP SYNC.          
004900 77  MAX-INDX                    PIC S9(4)   VALUE +3 COMP SYNC.          
005000     EJECT                                                                
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
007410*01  AREA -COPY W4267501     -PRE UT-                                     
007500     EJECT                                                                
007510 01  W426-AREA2-START             PIC X(24)   VALUE                       
007520                                 'W426-AREA2-START  '.                    
007530     SKIP2                                                                
007540                                                                          
007550*01  AREA -COPY W4267501     -PRE UT2-                                    
007560     EJECT                                                                
007570 01  W426-AREA3-START             PIC X(24)   VALUE                       
007580                                 'W426-AREA3-START  '.                    
007590     SKIP2                                                                
007591                                                                          
007592*01  AREA -COPY W4267701     -PRE UT3-                                    
007593     EJECT                                                                
007600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007700*                                                                         
007800     EJECT                                                                
007900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008000     SKIP3                                                                
008010*    --- STATUS-KOD FRÅN IMS                                              
008020 01  STATUS-WS                   PIC XX.                                  
008030     88  SEGMENT-FINNS                       VALUE '  '.                  
008040     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008041     88  SEGMENT-SLUT                        VALUE 'GB'.                  
008050     SKIP2                                                                
008060 01  GODK-STATUSKODER.                                                    
008070     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
008080     SKIP3                                                                
008090 01  SSA1                        PIC X(64).                               
008091 01  SSA2                        PIC X(64).                               
008092     EJECT                                                                
008093                                                                          
009500*    --- IMS FUNKTIONSKODER                                               
009600*01  -COPY W0003                                                          
009700     EJECT                                                                
009900*    ---  DLI INPUT-OUTPUT AREA                                           
010000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
010100     SKIP3                                                                
010200 01  DLI-IO-AREA.                                                         
010300     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
010401     SKIP3                                                                
010405     03  W6KVAH01 REDEFINES IO-AREA.                                      
010410*        05  -COPY W6D201  -PRE KVAH-                                     
010411     EJECT                                                                
010412 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
010413     SKIP3                                                                
010414 01  DLI-IO-AREA2.                                                        
010415     03  IO-AREA2                 PIC X(150)  VALUE SPACE.                
010416     SKIP3                                                                
010420     03  W6KVAH12 REDEFINES IO-AREA2.                                     
010430*        05  -COPY W6D212  -PRE KVAH-                                     
010700     EJECT                                                                
010800 LINKAGE SECTION.                                                         
010900                                                                          
011020*01  -COPY W0008  -PRE KVAH-                                              
011030     05  FILLER                PIC X.                                     
011100     EJECT                                                                
011201 PROCEDURE DIVISION  USING KVAH-PCB.                                      
011210     ENTRY 'DLITCBL' USING KVAH-PCB.                                      
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
013710     OPEN OUTPUT W42675                                                   
013720                 W42676                                                   
013730                 W42677                                                   
014010     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014200     .                                                                    
014300     EJECT                                                                
014301 B-BEHANDLA-INDATA SECTION.                                               
014302                                                                          
014310     PERFORM IMS-GN-KVAH01                                                
014312     PERFORM UNTIL SEGMENT-SLUT                                           
014317       PERFORM S11-SKRIV-W42675                                           
014318       PERFORM S12-SKRIV-W42676                                           
014319       PERFORM S01-KOLLA-KVALSAK                                          
014320       PERFORM IMS-GN-KVAH01                                              
014395     END-PERFORM                                                          
014407     .                                                                    
014408     EJECT                                                                
014459 S01-KOLLA-KVALSAK SECTION.                                               
014460                                                                          
014461     PERFORM IMS-GNP-KVAH12                                               
014462     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
014463       IF KVAH-LEV-FLKVASAK = JA                                          
014464         PERFORM S13-SKRIV-W42677                                         
014465       END-IF                                                             
014466       PERFORM IMS-GNP-KVAH12                                             
014467     END-PERFORM                                                          
014475     .                                                                    
014476     EJECT                                                                
014477 S11-SKRIV-W42675 SECTION.                                                
014478                                                                          
014479     MOVE KVAH-ART-IDARTNR        TO UT-IDARTNR                           
014480     MOVE KVAH-ART-ADKVAULG       TO UT-ADKVAULG                          
014481     MOVE KVAH-ART-KDKVAKTL       TO UT-KDKVAKTL                          
014482                                                                          
014483     WRITE UT-POST FROM UT-AREA                                           
014484     MOVE 'W42675' TO POSTSUM-FDNAMN                                      
014485     MOVE 'W42675D1' TO POSTSUM-DDNAMN2                                   
014486     CALL POSTSUM USING POSTSUM-PARM                                      
014487     .                                                                    
014488     EJECT                                                                
014489 S12-SKRIV-W42676 SECTION.                                                
014490                                                                          
014491     MOVE KVAH-ART-IDARTNR        TO UT2-IDARTNR                          
014492     MOVE KVAH-ART-ADKVAULG       TO UT2-ADKVAULG                         
014493     MOVE KVAH-ART-KDKVAKTL       TO UT2-KDKVAKTL                         
014494                                                                          
014495     WRITE UT2-POST FROM UT2-AREA                                         
014496     MOVE 'W42676' TO POSTSUM-FDNAMN                                      
014497     MOVE 'W42675D2' TO POSTSUM-DDNAMN2                                   
014498     CALL POSTSUM USING POSTSUM-PARM                                      
014499     .                                                                    
014500     EJECT                                                                
014501 S13-SKRIV-W42677 SECTION.                                                
014502                                                                          
014503     MOVE KVAH-ART-IDARTNR        TO UT3-IDARTNR                          
014504     MOVE KVAH-LEV-IDLEVNR        TO UT3-IDLEVNR                          
014505     MOVE KVAH-LEV-TIKVASAK       TO UT3-TIKVASAK                         
014506                                                                          
014507     WRITE UT3-POST FROM UT3-AREA                                         
014508     MOVE 'W42677' TO POSTSUM-FDNAMN                                      
014509     MOVE 'W42675D3' TO POSTSUM-DDNAMN2                                   
014510     CALL POSTSUM USING POSTSUM-PARM                                      
014511     .                                                                    
014512     EJECT                                                                
014513 Z-FINIT SECTION.                                                         
014514                                                                          
014515     CLOSE W42675                                                         
014520           W42676                                                         
014530           W42677                                                         
014602     MOVE 'S' TO POSTSUM-OPKOD                                            
014610     CALL POSTSUM USING POSTSUM-PARM                                      
014700     .                                                                    
014901     EJECT                                                                
015800* --- IMS SEKTIONER ---                                                   
015900     SKIP3                                                                
016020 IMS-GN-KVAH01 SECTION.                                                   
016023     MOVE 'W6KVAH01 ' TO SSA1                                             
016025     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
016026     CALL CBLTDLI USING GN KVAH-PCB DLI-IO-AREA SSA1                      
016027     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
016028     PERFORM IMS-STATUSKONTROLL                                           
016029     .                                                                    
016030     SKIP3                                                                
016040 IMS-GNP-KVAH12 SECTION.                                                  
016050     MOVE 'W6KVAH12 ' TO SSA1                                             
016060     MOVE '  GEGB' TO GODK-STATUSKODER                                    
016070     CALL CBLTDLI USING GNP KVAH-PCB DLI-IO-AREA2 SSA1                    
016080     MOVE KVAH-STATUS-CODE TO STATUS-WS                                   
016090     PERFORM IMS-STATUSKONTROLL                                           
016100     .                                                                    
016110     SKIP3                                                                
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
