000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6126100.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   01/08/17.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        NEDLÄSNING WDL6 VECKA                                            
000900*                                                                         
001000                                                                          
001100     SKIP3                                                                
001200 ENVIRONMENT DIVISION.                                                    
001300     SKIP2                                                                
001400 INPUT-OUTPUT SECTION.                                                    
001500                                                                          
001600 FILE-CONTROL.                                                            
001700     SKIP2                                                                
001800*          --- NEDLÄSNING WDL6 R32 VECKA                                  
001900     SELECT UTFIL                      ASSIGN TO W61261D1.                
002000     EJECT                                                                
002100 DATA DIVISION.                                                           
002200     SKIP2                                                                
002300 FILE SECTION.                                                            
002400     SKIP3                                                                
002500 FD  UTFIL                                                                
002600     RECORDING       F                                                    
002700     BLOCK CONTAINS  0.                                                   
002800                                                                          
002900*01  POST -COPY W61207 -PRE  UT-  -L.                                     
003000     EJECT                                                                
003100 WORKING-STORAGE SECTION.                                                 
003200                                                                          
003300 77  IDPGM                       PIC X(8)    VALUE 'W6126100'.            
003400 77  JA                          PIC X       VALUE 'J'.                   
003500 77  NEJ                         PIC X       VALUE 'N'.                   
003600                                                                          
003700 01  DAGENS-DATUM-VECKA          PIC 9(4)    VALUE ZERO.                  
003800 01  FILLER REDEFINES DAGENS-DATUM-VECKA.                                 
003900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004000     03  DAGENS-DATUM-VV         PIC 9(2).                                
004200                                                                          
004300 01  DYNAMISKA-SUBPROGRAM.                                                
004400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004600     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
004700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
004710     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
004800                                                                          
004900 01  FELTEXT.                                                             
005000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005200     EJECT                                                                
005300*    --- PARAMETRAR TILL DATKORT                                          
005400*                                                                         
005500 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W61261'.              
005700 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
005900*01  -COPY WDATKORT                                                       
006000     EJECT                                                                
006020*01  -COPY WDATAREA                                                       
006030     EJECT                                                                
006100*    --- PARAMETRAR TILL POSTSUM                                          
006200*                                                                         
006300*01  -COPY W0005   -PRE  POSTSUM-                                         
006400     EJECT                                                                
006800 01  UT-AREA-START               PIC X(24)   VALUE                        
006900                                 'UT-AREA-START  '.                       
007000                                                                          
007100*01  AREA -COPY W61207     -PRE UT-                                       
007200     EJECT                                                                
007300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007400*                                                                         
007500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007600                                                                          
007700 01  NYCKLAR-TILL-DLI.                                                    
007800     03  W-IDARTNR-X.                                                     
007900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008000                                                                          
008010     03  W-IDDC-B6-X.                                                     
008020         05 W-IDDC-B6                  PIC X(2).                          
008030                                                                          
008100*    --- STATUS-KOD FRÅN IMS                                              
008200 01  STATUS-WS                   PIC XX.                                  
008300     88  SEGMENT-FINNS                       VALUE '  '.                  
008400     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
008500                                                                          
008600 01  GODK-STATUSKODER.                                                    
008700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
008800     EJECT                                                                
008810 01  SSA1                        PIC X(160).                              
008900*    --- IMS FUNKTIONSKODER                                               
009000*01  -COPY W0003                                                          
009100     EJECT                                                                
009200*    ---  DLI INPUT-OUTPUT AREA                                           
009300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL6'.                        
009400 01  DLI-IO-WDL6.                                                         
009500     03 IO-AREA     PIC X(600) VALUE SPACE.                               
009600         03 DLI-IO-WDL601 REDEFINES IO-AREA.                              
009700*            05 -COPY WDL601                                              
009800     EJECT                                                                
009900         03 DLI-IO-WDL611 REDEFINES IO-AREA.                              
010000*            05 -COPY WDL611                                              
010010                                                                          
010020 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
010030 01   DLI-IO-AREA-B601.                                                   
010040*     03  -COPY WDB601                                                    
010050                                                                          
010100     EJECT                                                                
010200 LINKAGE SECTION.                                                         
010300                                                                          
010400*01  -COPY W0008  -PRE WDL6-                                              
010500     05  FILLER                  PIC X.                                   
010510                                                                          
010520*01  -COPY W0008  -PRE WDB6-                                              
010530     05  FILLER                  PIC X.                                   
010600     EJECT                                                                
010700 PROCEDURE DIVISION  USING WDL6-PCB WDB6-PCB.                             
010800 MAIN SECTION.                                                            
010900     ENTRY 'DLITCBL' USING WDL6-PCB WDB6-PCB.                             
011000                                                                          
011100     PERFORM A-INIT                                                       
011200                                                                          
011300     PERFORM IMS-GET-WDL6                                                 
011400     PERFORM UNTIL SEGMENT-SAKNAS                                         
011500       EVALUATE WDL6-SEG-NAME-FB                                          
011600         WHEN 'WDL601'                                                    
011700           MOVE ART-IDARTNR TO UT-IDARTNR                                 
011800         WHEN 'WDL611'                                                    
011900           PERFORM B-URVAL                                                
012000       END-EVALUATE                                                       
012100       PERFORM IMS-GET-WDL6                                               
012200     END-PERFORM                                                          
012210                                                                          
012220     PERFORM Z-FINIT                                                      
012230     MOVE ZERO TO RETURN-CODE                                             
012240     GOBACK                                                               
012250     .                                                                    
012260     EJECT                                                                
012270 A-INIT SECTION.                                                          
012280                                                                          
012290     OPEN OUTPUT UTFIL                                                    
012300                                                                          
012400     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
012500     MOVE D-AAR    TO DAGENS-DATUM-AAR                                    
012600     MOVE D-VECKA  TO DAGENS-DATUM-VV                                     
012800     MOVE IDPGM    TO POSTSUM-PROGNAMN                                    
012900     .                                                                    
013000     EJECT                                                                
013100 B-URVAL SECTION.                                                         
013200                                                                          
013300     MOVE INL-IDDC TO W-IDDC-B6                                           
013310     PERFORM IMS-GU-WDB601                                                
013410     IF DCS-FLBINNUT = JA                                                 
013500        IF INL-IDPTYP = 'R32'                                             
013700           IF INL-IDUSER-003 = SPACE                                      
013800              CONTINUE                                                    
013900           ELSE                                                           
013910              MOVE 'AAMMDD'     TO DAT-KDDATFORM                          
013920              MOVE INL-TIINLINL TO DAT-I-TIDATUM                          
013930              CALL WDATKONV USING DAT-KDDATFORM                           
013940                                  DAT-I-TIDATUM                           
013950                                  DAT-O-TIDATUM                           
013960                                  DAT-KDSVAR                              
013980              IF DAT-KDSVAR-OK                                            
013990                 IF DAT-TIAA = D-AAR AND DAT-TIVV = D-VECKA               
014000                    IF INL-KVANTMOT = ZERO AND INL-KVART-SKROT = 0        
014010                       CONTINUE                                           
014020                    ELSE                                                  
014021                       MOVE INL-ADLAGOMR    TO UT-ADLAGOMR                
014022                       MOVE INL-IDDC        TO UT-IDDC                    
014023                       MOVE INL-IDUSER-003  TO UT-IDUSER-003              
014024                       MOVE INL-KVANTMOT    TO UT-KVANTMOT                
014025                       MOVE INL-KVAVIS      TO UT-KVAVIS                  
014026                       MOVE INL-KVART-SKROT TO UT-KVART-SKROT             
014027                       MOVE INL-TIINLINL    TO UT-TIINLINL                
014028                       PERFORM S11-SKRIV-UTFIL                            
014029                    END-IF                                                
014030                 END-IF                                                   
014031              END-IF                                                      
014032           END-IF                                                         
014033        END-IF                                                            
014034     END-IF                                                               
014035     .                                                                    
014036     EJECT                                                                
014037 Z-FINIT SECTION.                                                         
014038                                                                          
014039     CLOSE UTFIL                                                          
014040                                                                          
014041     MOVE 'S' TO POSTSUM-OPKOD                                            
014042     CALL POSTSUM USING POSTSUM-PARM                                      
014050     .                                                                    
014060     SKIP3                                                                
014070 S11-SKRIV-UTFIL SECTION.                                                 
014080                                                                          
014090     WRITE UT-POST FROM UT-AREA                                           
014100                                                                          
014200     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
014300     MOVE 'UTFIL'    TO POSTSUM-FDNAMN                                    
014400     MOVE 'W61261D1' TO POSTSUM-DDNAMN2                                   
014500     CALL POSTSUM USING POSTSUM-PARM                                      
014600     .                                                                    
014700     EJECT                                                                
014800* --- IMS SEKTIONER ---                                                   
014900                                                                          
015000 IMS-GET-WDL6 SECTION.                                                    
015100     CALL CBLTDLI USING GN WDL6-PCB DLI-IO-WDL6                           
015200     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
015300     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
015400     PERFORM IMS-STATUSKONTROLL                                           
015500     .                                                                    
015600     SKIP3                                                                
015610 IMS-GU-WDB601    SECTION.                                                
015620     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
015630          DELIMITED BY SIZE INTO SSA1                                     
015640     MOVE '  GE' TO GODK-STATUSKODER                                      
015650     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
015660     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
015670     PERFORM IMS-STATUSKONTROLL                                           
015680     IF SEGMENT-SAKNAS                                                    
015690         MOVE SPACE TO DCS-KDDC                                           
015691     END-IF                                                               
015692     .                                                                    
015700 IMS-STATUSKONTROLL SECTION.                                              
015800     SET STATUS-IX TO 1                                                   
015900     SEARCH GODK-STATUS                                                   
016000       AT END                                                             
016100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
016200           DELIMITED BY SIZE INTO FELTEXT                                 
016300         DISPLAY FELTEXT                                                  
016400         CALL FELLOG                                                      
016500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
016600         CONTINUE                                                         
016700     END-SEARCH                                                           
016800     .                                                                    
