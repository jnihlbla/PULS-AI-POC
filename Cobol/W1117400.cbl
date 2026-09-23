001100 ID DIVISION.                                                             
001200                                                                          
001300 PROGRAM-ID.     W1117400.                                                
001400 AUTHOR.         FRONTEC, GÖTEBORG.                                       
001500 DATE-WRITTEN.   96/08/22.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800                                                                          
001900*    FUNKTION:                                                            
002000*        TAR BORT 9101-SEGMENT FRÅN WDG3-BASEN                            
002010*                                                                         
002100*        UTIFRÅN INFIL FRÅN W11170                                        
002210*        PROGRAMMET UPPDATERAR WLXXID (WDG3)                              
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
003502*          --- UPPDATERINGSPOSTER                                         
003503     SELECT W11174-IN                  ASSIGN TO W11174D1.                
003504     SKIP2                                                                
003505*          --- UPPDATERINGSPOSTER                                         
003510     SELECT W11174-UT                  ASSIGN TO W11174D2.                
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP3                                                                
004000 FILE SECTION.                                                            
004101     SKIP3                                                                
004102 FD  W11174-IN                                                            
004103     RECORDING       F                                                    
004104     BLOCK CONTAINS  0.                                                   
004105                                                                          
004106*01  -COPY W11123      -PRE  IN-  -L.                                     
004107     SKIP3                                                                
004108 FD  W11174-UT                                                            
004109     RECORDING       F                                                    
004110     BLOCK CONTAINS  0.                                                   
004111                                                                          
004120*01  POST -COPY W11123 -PRE  UT-  -L.                                     
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400     SKIP2                                                                
004401                                                                          
004410*    -- CHECKED BY WY2000                                                 
004500 77  IDPGM                       PIC X(8)    VALUE 'W1117400'.            
004600 77  JA                          PIC X       VALUE 'J'.                   
004700 77  NEJ                         PIC X       VALUE 'N'.                   
004800     SKIP2                                                                
004810 01  W-ANT-BORTTAG               PIC 9(4)    VALUE ZERO.                  
004820 01  W-MAX-BORTTAG               PIC 9(3)    VALUE 500.                   
004830     SKIP2                                                                
004900 01  FELTEXT.                                                             
005000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005301                                                                          
005302 77  W11174-EOF-SW               PIC X       VALUE 'N'.                   
005310     88  END-OF-W11174                       VALUE 'J'.                   
005600     EJECT                                                                
005700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005800 01  FILLER REDEFINES DAGENS-DATUM.                                       
005900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006200     EJECT                                                                
006300 01  DYNAMISKA-SUBPROGRAM.                                                
006400*                                                                         
006500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006710     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006801     EJECT                                                                
006802*    --- PARAMETRAR TILL POSTSUM                                          
006803*                                                                         
006810*01  -COPY W0005   -PRE  POSTSUM-                                         
007101     EJECT                                                                
007102 01  IN-AREA-START               PIC X(24)   VALUE                        
007103                                             'IN-AREA-START'.             
007104     SKIP2                                                                
007105                                                                          
007106*01  AREA -COPY W11123     -PRE IN-                                       
007107     EJECT                                                                
007108 01  UT-AREA-START               PIC X(24)   VALUE                        
007109                                             'UT-AREA-START'.             
007110     SKIP2                                                                
007111                                                                          
007120*01  AREA -COPY W11123     -PRE UT-                                       
007200*                                                                         
007300     EJECT                                                                
007400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007500     SKIP3                                                                
007600 01  NYCKLAR-TILL-DLI.                                                    
007700     03  W-WDG3KEY-X.                                                     
007701         05  FILLER              PIC X(4)    VALUE '9101'.                
007702         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
007705     03  W-IDARTNR-X.                                                     
007710         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
007800     SKIP2                                                                
007900*    --- STATUS-KOD FRÅN IMS                                              
008000 01  STATUS-WS                   PIC XX.                                  
008100     88  SEGMENT-FINNS                       VALUE '  '.                  
008200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008400     88  SEGMENT-SLUT                        VALUE 'GB'.                  
008500     88  IMS-EJ-OK                           VALUE 'XD'.                  
008600     SKIP2                                                                
008700 01  GODK-STATUSKODER.                                                    
008800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
008900     SKIP3                                                                
009000 01  SSA1                        PIC X(64).                               
009100 01  SSA2                        PIC X(64).                               
009200     EJECT                                                                
009300*    --- IMS FUNKTIONSKODER                                               
009400*01  -COPY W0003                                                          
009500     EJECT                                                                
009700*    ---  DLI INPUT-OUTPUT AREA                                           
009800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
009900     SKIP3                                                                
010000 01  DLI-IO-AREA.                                                         
010100     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
010201     SKIP3                                                                
010210     03  WLXXID11 REDEFINES IO-AREA.                                      
010220*        05  -COPY WDGX9102  -PRE 9101-                                   
010500     EJECT                                                                
010600 LINKAGE SECTION.                                                         
010700                                                                          
010800*01  -COPY W0009   -PRE MSG-                                              
010901     EJECT                                                                
010902*01  -COPY W0008  -PRE XXID-                                              
010910     05  FILLER                  PIC X.                                   
011200     EJECT                                                                
011301 PROCEDURE DIVISION  USING MSG-PCB XXID-PCB.                              
011302 MAIN SECTION.                                                            
011310     ENTRY 'DLITCBL' USING MSG-PCB XXID-PCB.                              
011400                                                                          
011600     SKIP2                                                                
011700     PERFORM A-INIT                                                       
011810     PERFORM S01-LAES-W11174                                              
011820                                                                          
011900     PERFORM UNTIL (   END-OF-W11174                                      
012000                    OR W-ANT-BORTTAG > W-MAX-BORTTAG )                    
012100                                                                          
012110       MOVE IN-IDARTNR TO W-IDARTNR                                       
012200       PERFORM IMS-GHU-XXID11                                             
012300       IF SEGMENT-FINNS                                                   
012400         PERFORM IMS-DLET-XXID                                            
012410         ADD +1        TO W-ANT-BORTTAG                                   
012500       END-IF                                                             
012600                                                                          
012610       PERFORM S01-LAES-W11174                                            
012700     END-PERFORM                                                          
012900                                                                          
012910     PERFORM B-SKRIV-OBEHANDLADE                                          
012920                                                                          
013000     PERFORM Z-FINIT                                                      
013100                                                                          
013200     MOVE ZERO TO RETURN-CODE                                             
013300     GOBACK                                                               
013400     .                                                                    
013500     EJECT                                                                
013600 A-INIT SECTION.                                                          
013700     SKIP2                                                                
013801                                                                          
013810     OPEN INPUT  W11174-IN                                                
013901                                                                          
013910     OPEN OUTPUT W11174-UT                                                
014100                                                                          
014300                                                                          
014410     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014700     .                                                                    
014810     EJECT                                                                
014820 B-SKRIV-OBEHANDLADE SECTION.                                             
014830***                                                                       
014840* SKRIVER DE POSTER SOM INTE HAR BEHANDLATS                               
014850* PÅ EN NY GENERATION AV FILEN W11174                                     
014860***                                                                       
014870                                                                          
014880     PERFORM UNTIL (END-OF-W11174)                                        
014890       MOVE IN-AREA TO UT-AREA                                            
014891                                                                          
014892       PERFORM S11-SKRIV-W11174                                           
014893       PERFORM S01-LAES-W11174                                            
014894     END-PERFORM                                                          
014895     .                                                                    
014896     EJECT                                                                
014900 Z-FINIT SECTION.                                                         
015000                                                                          
015101                                                                          
015102     CLOSE W11174-IN                                                      
015103           W11174-UT                                                      
015301     SKIP2                                                                
015302     MOVE 'S' TO POSTSUM-OPKOD                                            
015310     CALL POSTSUM USING POSTSUM-PARM                                      
015500     .                                                                    
015601     EJECT                                                                
015602 S01-LAES-W11174  SECTION.                                                
015603     SKIP2                                                                
015604     READ W11174-IN INTO IN-AREA                                          
015605     AT END                                                               
015607        SET END-OF-W11174 TO TRUE                                         
015608                                                                          
015609     NOT AT END                                                           
015610        MOVE 'W11174'   TO POSTSUM-FDNAMN                                 
015611        MOVE 'W11174D1' TO POSTSUM-DDNAMN2                                
015612        MOVE IN-IDPTYP  TO POSTSUM-TRANSTYP                               
015613        CALL POSTSUM USING POSTSUM-PARM                                   
015614     END-READ                                                             
015620     .                                                                    
015701     EJECT                                                                
015702 S11-SKRIV-W11174 SECTION.                                                
015703     SKIP2                                                                
015704     WRITE UT-POST FROM UT-AREA                                           
015705                                                                          
015706     MOVE UT-IDPTYP     TO POSTSUM-TRANSTYP                               
015707     MOVE 'W11174 '     TO POSTSUM-FDNAMN                                 
015708     MOVE 'W11174D2'    TO POSTSUM-DDNAMN2                                
015709     CALL POSTSUM USING POSTSUM-PARM                                      
015710     .                                                                    
015900     EJECT                                                                
016000* --- IMS SEKTIONER ---                                                   
016100     SKIP3                                                                
016202     EJECT                                                                
016203 IMS-GHU-XXID11 SECTION.                                                  
016204                                                                          
016205     STRING 'WLXXID01(WDG3KEY  =' W-WDG3KEY-X ')'                         
016206          DELIMITED BY SIZE INTO SSA1                                     
016207     STRING 'WLXXID11(IDARTNR  =' W-IDARTNR-X ')'                         
016208          DELIMITED BY SIZE INTO SSA2                                     
016210     MOVE '  ' TO GODK-STATUSKODER                                        
016211     CALL CBLTDLI USING GHU XXID-PCB DLI-IO-AREA SSA1 SSA2                
016212     MOVE XXID-STATUS-CODE TO STATUS-WS                                   
016213     PERFORM IMS-STATUSKONTROLL                                           
016214     .                                                                    
016222     SKIP3                                                                
016223 IMS-DLET-XXID SECTION.                                                   
016224                                                                          
016225     MOVE '  ' TO GODK-STATUSKODER                                        
016226     CALL CBLTDLI USING DLET XXID-PCB DLI-IO-AREA                         
016227     MOVE XXID-STATUS-CODE TO STATUS-WS                                   
016228     PERFORM IMS-STATUSKONTROLL                                           
016229     .                                                                    
016300     EJECT                                                                
016400 IMS-STATUSKONTROLL SECTION.                                              
016500     SKIP2                                                                
016600     SET STATUS-IX TO 1                                                   
016700     SEARCH GODK-STATUS                                                   
016800       AT END                                                             
016900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
017000           DELIMITED BY SIZE INTO FELTEXT                                 
017100         DISPLAY FELTEXT                                                  
017200         CALL FELLOG                                                      
017300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
017400         CONTINUE                                                         
017500     END-SEARCH                                                           
017600     .                                                                    
