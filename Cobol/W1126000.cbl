001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W1126000.                                                
001300 AUTHOR.         BODIL LINDAHL.                                           
001400 DATE-WRITTEN.   02/02/18.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNKTION:                                                            
001800*        NEDLÄSNING ARTIKLAR MED FÖRÄNDRAD FLIART                         
001900*                                                                         
002010*        PROGRAMMET LÄSER      WDK6                                       
002100*                                                                         
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003301     SKIP2                                                                
003302*          --- ARTINFO(0)                                                 
003303     SELECT W01177-NEW                 ASSIGN TO W11260D1.                
003304     SKIP2                                                                
003305*          --- ARTINFO(-1)                                                
003306     SELECT W01177-OLD                 ASSIGN TO W11260D2.                
003307     SKIP2                                                                
003309*          --- LISTFIL                                                    
003310     SELECT W11260                     ASSIGN TO W11260D3.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003901     SKIP3                                                                
003902 FD  W01177-NEW                                                           
003903     RECORDING       F                                                    
003904     BLOCK CONTAINS  0.                                                   
003906*01  -COPY W011100     -PRE  NEW- -L.                                     
003907     SKIP3                                                                
003908 FD  W01177-OLD                                                           
003909     RECORDING       F                                                    
003910     BLOCK CONTAINS  0.                                                   
003911*01  -COPY W011100     -PRE  OLD- -L.                                     
003912     SKIP3                                                                
003913 FD  W11260                                                               
003914     RECORDING       F                                                    
003915     BLOCK CONTAINS  0.                                                   
003920*01  POST -COPY W11260 -PRE  UT-  -L.                                     
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'W1126000'.            
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004701                                                                          
004702 77  W01177-NEW-EOF-SW           PIC X       VALUE 'N'.                   
004710     88  END-OF-W01177-NEW                   VALUE 'J'.                   
004800                                                                          
004810 77  W01177-OLD-EOF-SW           PIC X       VALUE 'N'.                   
004820     88  END-OF-W01177-OLD                   VALUE 'J'.                   
004830                                                                          
004900 01  DAGENS-VECKA                PIC 9(4)    VALUE ZERO.                  
005000 01  FILLER REDEFINES DAGENS-VECKA.                                       
005100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005200     03  DAGENS-DATUM-VECKA      PIC 9(2).                                
005400                                                                          
005500 01  DYNAMISKA-SUBPROGRAM.                                                
005700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006000     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
006110     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006200                                                                          
006300*    --- PARAMETRAR TILL ABEND                                            
006500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
006700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
006800                                                                          
006900 01  FELTEXT.                                                             
007000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007200     EJECT                                                                
007300*    --- PARAMETRAR TILL DATKORT                                          
007500 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W11260'.              
007600     SKIP2                                                                
007700 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
007800     SKIP2                                                                
007900*01  -COPY WDATKORT                                                       
008001     EJECT                                                                
008002*    --- PARAMETRAR TILL POSTSUM                                          
008010*01  -COPY W0005   -PRE  POSTSUM-                                         
008201     EJECT                                                                
008202 01  IN-AREA-NEW-START            PIC X(24)   VALUE                       
008203                                 'IN-AREA-NEW-START'.                     
008204     SKIP2                                                                
008206*01  AREA -COPY W011100    -PRE NEW-                                      
008207     EJECT                                                                
008208 01  IN-AREA-OLD-START            PIC X(24)   VALUE                       
008209                                 'IN-AREA-OLD-START'.                     
008210     SKIP2                                                                
008211*01  AREA -COPY W011100    -PRE OLD-                                      
008212     EJECT                                                                
008213 01  UT-AREA-START               PIC X(24)   VALUE                        
008214                                 'UT-AREA-START  '.                       
008215     SKIP2                                                                
008220*01  AREA -COPY W11260     -PRE UT-                                       
008300     EJECT                                                                
008400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008800                                                                          
008900 01  NYCKLAR-TILL-DLI.                                                    
009001     03  W-IDARTNR-X.                                                     
009002         05  W-IDARTNR      PIC S9(9) VALUE ZERO COMP-3.                  
009100     SKIP3                                                                
009200*    --- STATUS-KOD FRÅN IMS                                              
009300 01  STATUS-WS                   PIC XX.                                  
009400     88  SEGMENT-FINNS                       VALUE '  '.                  
009500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009700     SKIP3                                                                
009800 01  GODK-STATUSKODER.                                                    
009900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010000     SKIP3                                                                
010100 01  SSA1                        PIC X(64).                               
010200 01  SSA2                        PIC X(64).                               
010300     EJECT                                                                
010400*    --- IMS FUNKTIONSKODER                                               
010500*01  -COPY W0003                                                          
010600     EJECT                                                                
010800*    ---  DLI INPUT-OUTPUT AREA                                           
010905 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
010906 01  DLI-IO-WDK611.                                                       
010910*    03  -COPY WDK611                                                     
011200     EJECT                                                                
011300 LINKAGE SECTION.                                                         
011501                                                                          
011502*01  -COPY W0008  -PRE WDK6-                                              
011503     05  FILLER                  PIC X.                                   
011504     EJECT                                                                
011701 PROCEDURE DIVISION  USING WDK6-PCB.                                      
011702 MAIN SECTION.                                                            
011710     ENTRY 'DLITCBL' USING WDK6-PCB.                                      
012000                                                                          
012100     PERFORM A-INIT                                                       
012101                                                                          
012110     PERFORM S01-LAES-W01177-NEW                                          
012120     PERFORM S02-LAES-W01177-OLD                                          
012130                                                                          
012140     PERFORM UNTIL END-OF-W01177-NEW AND                                  
012150                   END-OF-W01177-OLD                                      
012160                                                                          
012170        IF NEW-IDARTNR = OLD-IDARTNR                                      
012180           IF NEW-FLIART NOT = OLD-FLIART                                 
012192              PERFORM B-SKAPA-SKRIV-UTFIL                                 
012194           END-IF                                                         
012195           PERFORM S01-LAES-W01177-NEW                                    
012196           PERFORM S02-LAES-W01177-OLD                                    
012197        ELSE                                                              
012200           IF NEW-IDARTNR > OLD-IDARTNR                                   
012201*****  OLD-ARTNR FINNES EJ PÅ NYA LAGERBANDET                             
012202              PERFORM S02-LAES-W01177-OLD                                 
012203           ELSE                                                           
012204*****  NEW-ARTNR FINNS EJ PÅ GAMLA LAGERBANDET = NYTT ARTNR               
012205              IF NEW-FLIART = JA                                          
012210                 PERFORM B-SKAPA-SKRIV-UTFIL                              
012211              END-IF                                                      
012212              PERFORM S01-LAES-W01177-NEW                                 
012213           END-IF                                                         
012230        END-IF                                                            
012240     END-PERFORM                                                          
012250                                                                          
013500     PERFORM Z-FINIT                                                      
013700     MOVE ZERO TO RETURN-CODE                                             
013800     GOBACK                                                               
013900     .                                                                    
014000     EJECT                                                                
014100 A-INIT SECTION.                                                          
014200                                                                          
014210     OPEN INPUT  W01177-NEW                                               
014220                 W01177-OLD                                               
014310     OPEN OUTPUT W11260                                                   
014400                                                                          
014500     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
014600     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
014700     MOVE D-VECKA     TO DAGENS-DATUM-VECKA                               
014910     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014920                                                                          
014930     PERFORM S98-NOLLSTALL                                                
015100     .                                                                    
015200     EJECT                                                                
015210 B-SKAPA-SKRIV-UTFIL SECTION.                                             
015211                                                                          
015212     MOVE NEW-IDARTNR TO W-IDARTNR                                        
015216                                                                          
015217     MOVE NEW-IDARTNR  TO UT-IDARTNR                                      
015218     MOVE NEW-FLIART   TO UT-FLIART                                       
015219     MOVE NEW-ADLAGOMR TO UT-ADLAGOMR                                     
015220     MOVE NEW-ADGANG   TO UT-ADGANG                                       
015221     MOVE NEW-ADPLATS  TO UT-ADPLATS                                      
015222     MOVE DAGENS-VECKA      TO UT-TIAAVV                                  
015223                                                                          
015224     PERFORM IMS-GET-WDK611                                               
015225     IF SEGMENT-FINNS                                                     
015226        MOVE CLAG-ADLAGOMR-SVS TO UT-ADLAGOMR-SVS                         
015227        MOVE CLAG-ADGANG-SVS   TO UT-ADGANG-SVS                           
015228        MOVE CLAG-ADPLATS-SVS  TO UT-ADPLATS-SVS                          
015229     END-IF                                                               
015230     PERFORM S11-SKRIV-W11260                                             
015240     PERFORM S98-NOLLSTALL                                                
015245     .                                                                    
015250     EJECT                                                                
015300 Z-FINIT SECTION.                                                         
015400                                                                          
015401     CLOSE W01177-NEW                                                     
015410           W01177-OLD                                                     
015420           W11260                                                         
015501                                                                          
015502     MOVE 'S' TO POSTSUM-OPKOD                                            
015510     CALL POSTSUM USING POSTSUM-PARM                                      
015600     .                                                                    
015701     EJECT                                                                
015702 S01-LAES-W01177-NEW SECTION.                                             
015703                                                                          
015714     READ W01177-NEW INTO NEW-AREA                                        
015715     AT END                                                               
015716        MOVE 999999999 TO NEW-IDARTNR                                     
015717        SET END-OF-W01177-NEW TO TRUE                                     
015718     NOT AT END                                                           
015719        MOVE 'W01177'   TO POSTSUM-FDNAMN                                 
015720        MOVE 'W11260D1' TO POSTSUM-DDNAMN2                                
015721        MOVE SPACE      TO POSTSUM-TRANSTYP                               
015722        CALL POSTSUM USING POSTSUM-PARM                                   
015723     END-READ                                                             
015730     .                                                                    
015801     SKIP3                                                                
015802 S02-LAES-W01177-OLD SECTION.                                             
015803                                                                          
015804     READ W01177-OLD INTO OLD-AREA                                        
015805     AT END                                                               
015806        MOVE 999999999 TO OLD-IDARTNR                                     
015807        SET END-OF-W01177-OLD TO TRUE                                     
015808     NOT AT END                                                           
015809        MOVE 'W01177'   TO POSTSUM-FDNAMN                                 
015810        MOVE 'W11260D2' TO POSTSUM-DDNAMN2                                
015811        MOVE SPACE      TO POSTSUM-TRANSTYP                               
015812        CALL POSTSUM USING POSTSUM-PARM                                   
015813     END-READ                                                             
015814     .                                                                    
015815     EJECT                                                                
015816 S11-SKRIV-W11260 SECTION.                                                
015817                                                                          
015818     WRITE UT-POST FROM UT-AREA                                           
015819                                                                          
015820     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
015821     MOVE 'W11260'   TO POSTSUM-FDNAMN                                    
015822     MOVE 'W11260D3' TO POSTSUM-DDNAMN2                                   
015823     CALL POSTSUM USING POSTSUM-PARM                                      
015830     .                                                                    
016000     EJECT                                                                
016010 S98-NOLLSTALL SECTION.                                                   
016020                                                                          
016030     MOVE ZERO  TO UT-IDARTNR                                             
016040                   UT-FLIART                                              
016041                   UT-ADLAGOMR                                            
016042                   UT-ADGANG                                              
016043                   UT-ADPLATS                                             
016044                   UT-ADLAGOMR-SVS                                        
016045                   UT-ADGANG-SVS                                          
016046                   UT-ADPLATS-SVS                                         
016048                   UT-TIAAVV                                              
016049     MOVE SPACE TO UT-FLIART                                              
016090     .                                                                    
016091     EJECT                                                                
016700* --- IMS SEKTIONER ---                                                   
016800                                                                          
016902 IMS-GET-WDK611 SECTION.                                                  
016904     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
016905          DELIMITED BY SIZE INTO SSA1                                     
016906     MOVE 'WDK611  ' TO SSA2                                              
016907     MOVE '  GE' TO GODK-STATUSKODER                                      
016908     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
016909     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
016910     PERFORM IMS-STATUSKONTROLL                                           
016911     .                                                                    
017000     EJECT                                                                
017200 IMS-STATUSKONTROLL SECTION.                                              
017300     SET STATUS-IX TO 1                                                   
017400     SEARCH GODK-STATUS                                                   
017500       AT END                                                             
017600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
017700           DELIMITED BY SIZE INTO FELTEXT                                 
017800         DISPLAY FELTEXT                                                  
017900         CALL FELLOG                                                      
018000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
018100         CONTINUE                                                         
018200     END-SEARCH                                                           
018300     .                                                                    
