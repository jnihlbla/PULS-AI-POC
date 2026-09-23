001000 ID DIVISION.                                                             
001200 PROGRAM-ID.     W4791200.                                                
001300 AUTHOR.         LASSI OLGRENER.                                          
001400 DATE-WRITTEN.   DEC 1999.                                                
001410 DATE-COMPILED.                                                           
001500                                                                          
001800*    FUNKTION:                                                            
001900*        SORTERAR INFILER I SAMMA ORDNING SOM WDE6                        
002000*        VIA W015RAND.                                                    
002300*                                                                         
002800                                                                          
003000 ENVIRONMENT DIVISION.                                                    
003100                                                                          
003200 INPUT-OUTPUT SECTION.                                                    
003300                                                                          
003400 FILE-CONTROL.                                                            
003501                                                                          
003502*          --- RENSNINGSPOSTER TILL WDE4.                                 
003510     SELECT W47910                     ASSIGN TO W47912D1.                
003600                                                                          
003601*          --- RENSNINGSPOSTER TILL WDE6.                                 
003602     SELECT W47912                     ASSIGN TO W47912D2.                
003603                                                                          
003640*          --- SORTERADE RENSNINGSPOSTER TILL WDE4.                       
003650     SELECT W479E4                     ASSIGN TO W47912D4.                
003660                                                                          
003661*          --- SORTERADE RENSNINGSPOSTER TILL WDE6.                       
003662     SELECT W479E6                     ASSIGN TO W47912D5.                
003663                                                                          
003700*          --- SORTERINGSFIL E4                                           
003800     SELECT SORTFIL                    ASSIGN TO W47912DZ.                
003900                                                                          
003901*          --- SORTERINGSFIL E6                                           
003902     SELECT SORTFIL1                   ASSIGN TO W47912DS.                
004000     EJECT                                                                
004100 DATA DIVISION.                                                           
004200                                                                          
004300 FILE SECTION.                                                            
004401                                                                          
004402 FD  W47910                                                               
004403     RECORDING       F                                                    
004405     BLOCK CONTAINS  0.                                                   
004406                                                                          
004410*01  -COPY W479010      -L.                                               
004500                                                                          
004501 FD  W47912                                                               
004502     RECORDING       F                                                    
004503     BLOCK CONTAINS  0.                                                   
004504                                                                          
004505*01  -COPY W479012      -L.                                               
004506                                                                          
004570 FD  W479E4                                                               
004580     RECORDING       F                                                    
004591     BLOCK CONTAINS  0.                                                   
004592                                                                          
004593*01  UTPOST -COPY W479E4       -L.                                        
004594                                                                          
004595 FD  W479E6                                                               
004596     RECORDING       F                                                    
004597     BLOCK CONTAINS  0.                                                   
004598                                                                          
004599*01  UTPOST1 -COPY W479E6       -L.                                       
004600                                                                          
004610 SD  SORTFIL.                                                             
004701                                                                          
004702 01  SORT-POST.                                                           
004720     03  -COPY W479E4           -PRE SORT-.                               
004730                                                                          
004731 SD  SORTFIL1.                                                            
004732                                                                          
004733 01  SORT1-POST.                                                          
004735     03  -COPY W479E6           -PRE SORT1-.                              
004800     EJECT                                                                
004900 WORKING-STORAGE SECTION.                                                 
005001                                                                          
005100 77  IDPGM                       PIC X(8)    VALUE 'W4791200'.            
005200 77  JA                          PIC X       VALUE 'J'.                   
005300 77  NEJ                         PIC X       VALUE 'N'.                   
005508                                                                          
005509 77  W47910-EOF-SW               PIC X       VALUE 'N'.                   
005510     88  END-OF-W47910                       VALUE 'J'.                   
005600                                                                          
005601 77  W47912-EOF-SW               PIC X       VALUE 'N'.                   
005602     88  END-OF-W47912                       VALUE 'J'.                   
005603                                                                          
005700 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
005800     88  END-OF-SORTFIL                      VALUE 'J'.                   
005810                                                                          
005811 77  SORTFIL1-EOF-SW             PIC X       VALUE 'N'.                   
005812     88  END-OF-SORTFIL1                     VALUE 'J'.                   
005900     EJECT                                                                
006600 01  DYNAMISKA-SUBPROGRAM.                                                
006700*                                                                         
006800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007110     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007120     03  W015RAND                PIC X(8)    VALUE 'W015RAND'.            
007200     EJECT                                                                
007300*    --- PARAMETRAR TILL ABEND                                            
007400                                                                          
007500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007700                                                                          
007800 01  FELTEXT.                                                             
007900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008101     EJECT                                                                
008102*    --- PARAMETRAR TILL POSTSUM                                          
008103*                                                                         
008110*01  -COPY W0005   -PRE  POSTSUM-                                         
008120                                                                          
008301     EJECT                                                                
008302*    --- PARAMETRAR TILL W015RAND                                         
008303                                                                          
008304 01  SORT-NYCKEL.                                                         
008307     03  IDGMTREF                PIC X(17).                               
008308     03  IDPRODNR-E4             PIC S9(7) COMP-3.                        
008309     03  IDPLKLST                PIC S9(3) COMP-3.                        
008310 01  SORT1-NYCKEL.                                                        
008311     03  IDPRODNR                PIC S9(7) COMP-3.                        
008317 01  SORT-DBD                    PIC X(4)  VALUE SPACE.                   
008318 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
008319                                                                          
008320     EJECT                                                                
008321 01  IN-AREA-START                PIC X(16)   VALUE                       
008322                                 'IN-AREA-START '.                        
008323                                                                          
008330*01  AREA -COPY W479010     -PRE IN-                                      
008401     EJECT                                                                
008402 01  IN1-AREA-START               PIC X(16)   VALUE                       
008403                                 'IN1-AREA-START '.                       
008404                                                                          
008405*01  AREA -COPY W479012     -PRE IN1-                                     
008406     EJECT                                                                
008412 01  UT-AREA-START                PIC X(16)   VALUE                       
008413                                 'UT-AREA-START '.                        
008414                                                                          
008415*01  AREA -COPY W479E4      -PRE UT-                                      
008416     EJECT                                                                
008417 01  UT1-AREA-START               PIC X(16)   VALUE                       
008418                                 'UT1-AREA-START '.                       
008419                                                                          
008420*01  AREA -COPY W479E6      -PRE UT1-                                     
008421     EJECT                                                                
008427 01  SORTWS-AREA-START           PIC X(16)   VALUE                        
008428                                  'SORTWS-AREA '.                         
008429                                                                          
008430*01  AREA -COPY W479E4     -PRE SORTWS-                                   
008600     EJECT                                                                
008610 01  SORTWS1-AREA-START          PIC X(16)   VALUE                        
008620                                  'SORTWS1-AREA '.                        
008630                                                                          
008640*01  AREA -COPY W479E6     -PRE SORTWS1-                                  
008650     EJECT                                                                
012403 PROCEDURE DIVISION.                                                      
012404 MAIN SECTION.                                                            
012500                                                                          
012800     PERFORM A-INIT                                                       
012900                                                                          
013000     SORT SORTFIL  ASCENDING KEY SORT-IDRANDOM                            
013200                   INPUT PROCEDURE B-SORT-INPUT                           
013300                   OUTPUT PROCEDURE C-SORT-OUTPUT                         
013400                                                                          
013410     IF SORT-RETURN NOT = 0                                               
013420       MOVE SORT-RETURN TO SORT-RETURN-X                                  
013430       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
013431           DELIMITED BY SIZE                                              
013432           INTO FELTEXT-STR                                               
013433       DISPLAY FELTEXT                                                    
013434       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
013435     END-IF                                                               
013436                                                                          
013437     CANCEL W015RAND                                                      
013438     SORT SORTFIL1 ASCENDING KEY SORT1-IDRANDOM                           
013439                   INPUT PROCEDURE D-SORT1-INPUT                          
013440                   OUTPUT PROCEDURE E-SORT1-OUTPUT                        
013441                                                                          
013442     IF SORT-RETURN NOT = 0                                               
013443       MOVE SORT-RETURN TO SORT-RETURN-X                                  
013444       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT1'                     
013445           DELIMITED BY SIZE                                              
013446           INTO FELTEXT-STR                                               
013447       DISPLAY FELTEXT                                                    
013448       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
013449     ELSE                                                                 
014300       PERFORM Z-FINIT                                                    
014500       MOVE ZERO TO RETURN-CODE                                           
014600       GOBACK                                                             
014700     END-IF                                                               
014900     .                                                                    
015000     EJECT                                                                
015100 A-INIT SECTION.                                                          
015201                                                                          
015210     OPEN INPUT  W47910                                                   
015211                 W47912                                                   
015230     OPEN OUTPUT W479E4                                                   
015231                 W479E6                                                   
015400                                                                          
015610     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015800     .                                                                    
015901     EJECT                                                                
015902 B-SORT-INPUT  SECTION.                                                   
015903                                                                          
015906     PERFORM S01-LAES-W47910                                              
015907     PERFORM UNTIL END-OF-W47910                                          
015908       MOVE IN-IDGMTREF  TO SORTWS-IDGMTREF                               
015910                            IDGMTREF                                      
015913       MOVE IN-IDPRODNR  TO SORTWS-IDPRODNR                               
015914                            IDPRODNR-E4                                   
015915       MOVE IN-IDPLKLST  TO SORTWS-IDPLKLST                               
015916                            IDPLKLST                                      
015917       MOVE 'WDE4'       TO SORT-DBD                                      
015918       CALL W015RAND USING SORT-NYCKEL SORTWS-IDRANDOM SORT-DBD           
015919       PERFORM S31-SORT-RELEASE                                           
015920                                                                          
015921       PERFORM S01-LAES-W47910                                            
015922     END-PERFORM                                                          
015930     .                                                                    
016000     EJECT                                                                
016100 C-SORT-OUTPUT SECTION.                                                   
016200                                                                          
016300     PERFORM S32-SORT-RETURN                                              
016400     PERFORM UNTIL END-OF-SORTFIL                                         
016419        MOVE SORTWS-AREA      TO UT-AREA                                  
016423                                                                          
016424        PERFORM S04-SKRIV-W479E4                                          
016425                                                                          
016740        PERFORM S32-SORT-RETURN                                           
016800     END-PERFORM                                                          
016900     .                                                                    
017000     EJECT                                                                
017010 D-SORT1-INPUT  SECTION.                                                  
017020                                                                          
017030     PERFORM S02-LAES-W47912                                              
017040     PERFORM UNTIL END-OF-W47912                                          
017050       MOVE IN1-IDPRODNR TO SORTWS1-IDPRODNR                              
017060                            IDPRODNR                                      
017070       MOVE 'WDE6'       TO SORT-DBD                                      
017080       CALL W015RAND USING SORT1-NYCKEL SORTWS1-IDRANDOM SORT-DBD         
017090       PERFORM S33-SORT1-RELEASE                                          
017091                                                                          
017092       PERFORM S02-LAES-W47912                                            
017093     END-PERFORM                                                          
017094     .                                                                    
017095     EJECT                                                                
017096 E-SORT1-OUTPUT SECTION.                                                  
017097                                                                          
017098     PERFORM S34-SORT1-RETURN                                             
017099     PERFORM UNTIL END-OF-SORTFIL1                                        
017100        MOVE SORTWS1-AREA      TO UT1-AREA                                
017101                                                                          
017102        PERFORM S05-SKRIV-W479E6                                          
017103                                                                          
017104        PERFORM S34-SORT1-RETURN                                          
017105     END-PERFORM                                                          
017106     .                                                                    
017107     EJECT                                                                
017109                                                                          
017150 Z-FINIT SECTION.                                                         
017200                                                                          
017210     CLOSE W47910                                                         
017211           W47912                                                         
017230           W479E4                                                         
017231           W479E6                                                         
017301                                                                          
017302     MOVE 'S' TO POSTSUM-OPKOD                                            
017310     CALL POSTSUM USING POSTSUM-PARM                                      
017400     .                                                                    
017501     EJECT                                                                
017502 S01-LAES-W47910  SECTION.                                                
017503                                                                          
017504     READ W47910 INTO IN-AREA                                             
017505     AT END                                                               
017507        SET END-OF-W47910 TO TRUE                                         
017508                                                                          
017509     NOT AT END                                                           
017510        MOVE 'W47910'   TO POSTSUM-FDNAMN                                 
017511        MOVE 'W47912D1' TO POSTSUM-DDNAMN2                                
017512        MOVE 'WDE4'     TO POSTSUM-TRANSTYP                               
017513        CALL POSTSUM USING POSTSUM-PARM                                   
017514     END-READ                                                             
017520     .                                                                    
017800     EJECT                                                                
017801 S02-LAES-W47912  SECTION.                                                
017802                                                                          
017803     READ W47912 INTO IN1-AREA                                            
017804     AT END                                                               
017805        SET END-OF-W47912 TO TRUE                                         
017806                                                                          
017807     NOT AT END                                                           
017808        MOVE 'W47912'   TO POSTSUM-FDNAMN                                 
017809        MOVE 'W47912D2' TO POSTSUM-DDNAMN2                                
017810        MOVE 'WDE6'     TO POSTSUM-TRANSTYP                               
017811        CALL POSTSUM USING POSTSUM-PARM                                   
017812     END-READ                                                             
017813     .                                                                    
017814     EJECT                                                                
017896 S04-SKRIV-W479E4  SECTION.                                               
017897                                                                          
017898     WRITE UTPOST       FROM UT-AREA                                      
017899                                                                          
017904     MOVE 'W479E4'   TO POSTSUM-FDNAMN                                    
017905     MOVE 'W47912D4' TO POSTSUM-DDNAMN2                                   
017906     MOVE 'UT- '     TO POSTSUM-TRANSTYP                                  
017907     CALL POSTSUM USING POSTSUM-PARM                                      
017909     .                                                                    
017910     EJECT                                                                
017911 S05-SKRIV-W479E6  SECTION.                                               
017912                                                                          
017913     WRITE UTPOST1      FROM UT1-AREA                                     
017914                                                                          
017915     MOVE 'W479E6'   TO POSTSUM-FDNAMN                                    
017916     MOVE 'W47912D5' TO POSTSUM-DDNAMN2                                   
017917     MOVE 'UT1-'     TO POSTSUM-TRANSTYP                                  
017918     CALL POSTSUM USING POSTSUM-PARM                                      
017919     .                                                                    
017920     EJECT                                                                
017940 S31-SORT-RELEASE  SECTION.                                               
018000                                                                          
018100     RELEASE SORT-POST FROM SORTWS-AREA                                   
018200     .                                                                    
018300     EJECT                                                                
018400 S32-SORT-RETURN  SECTION.                                                
018500                                                                          
018600     RETURN SORTFIL INTO SORTWS-AREA                                      
018700     AT END                                                               
018800         SET END-OF-SORTFIL TO TRUE                                       
018900     .                                                                    
019000     EJECT                                                                
019010 S33-SORT1-RELEASE  SECTION.                                              
019020                                                                          
019030     RELEASE SORT1-POST FROM SORTWS1-AREA                                 
019040     .                                                                    
019050     EJECT                                                                
019060 S34-SORT1-RETURN  SECTION.                                               
019070                                                                          
019080     RETURN SORTFIL1 INTO SORTWS1-AREA                                    
019090     AT END                                                               
019091         SET END-OF-SORTFIL1 TO TRUE                                      
019092     .                                                                    
019093     EJECT                                                                
