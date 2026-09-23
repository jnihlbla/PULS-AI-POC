000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6121E00.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   07/08/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        NEDLÄSNING WDL6 VECKA PÅ FIL.                                    
000900*        SKAPAR ÄVEN EN FIL MED EXTERNA INLEVERANSER                      
001000*        FÖR USA OCH KINA (INPUT TILL W611V1 OCH ANVÄNDS                  
001100*        NÄR MAN SKALL SKAPA TOTALFIL TILL VIR).                          
001200*                                                                         
001300                                                                          
001400     SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*          --- NEDLÄSNING WDL6 R32 VECKA                                  
002200     SELECT UTFIL                      ASSIGN TO W6121ED1.                
002300*          --- EXTERNA INLEVERANSER USA/KINA  (TILL VIR SYSTEMET)         
002400     SELECT UTFIL2                     ASSIGN TO W6121ED2.                
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP2                                                                
002800 FILE SECTION.                                                            
002900     SKIP3                                                                
003000 FD  UTFIL                                                                
003100     RECORDING       F                                                    
003200     BLOCK CONTAINS  0.                                                   
003300                                                                          
003400*01  POST -COPY W6121E   -PRE  UT-  -L.                                   
003500     SKIP3                                                                
003600 FD  UTFIL2                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900                                                                          
004000*01  POST -COPY PI30INL2 -PRE  UT2- -L.                                   
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004400 77  IDPGM                       PIC X(8)    VALUE 'W6121E00'.            
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004702 77  SPAR-IDARTNR                PIC S9(9)   COMP-3 VALUE ZERO.           
004802 77  IX                          PIC S9(3)   COMP-3 VALUE ZERO.           
004902 77  TAB-DC-MAX                  PIC S9(3)   COMP-3 VALUE 100.            
005002                                                                          
005102 01  DAGENS-DATUM-VECKA          PIC 9(4)    VALUE ZERO.                  
005202 01  FILLER REDEFINES DAGENS-DATUM-VECKA.                                 
005302     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005402     03  DAGENS-DATUM-VV         PIC 9(2).                                
005502                                                                          
005602 01 WS-IDDC-TABELL.                                                       
005702    03 WS-VALID-IDDC  OCCURS 100 INDEXED BY WS-IDDC-IX.                   
005802       05 TAB-IDDC             PIC X(2).                                  
005902       05 TAB-IDLEVNR          PIC X(5).                                  
006002                                                                          
006102 01  DYNAMISKA-SUBPROGRAM.                                                
006202     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006302     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006402     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
006502     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006602     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006702                                                                          
006802 01  FELTEXT.                                                             
006902     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007002     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007102     EJECT                                                                
007202*    --- PARAMETRAR TILL DATKORT                                          
007302*                                                                         
007402 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W6121E'.              
007502 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
007602*01  -COPY WDATKORT                                                       
007702     EJECT                                                                
007802*01  -COPY WDATAREA                                                       
007902     EJECT                                                                
008002*01  -COPY WWDC99                                                         
008102     EJECT                                                                
008202*    --- PARAMETRAR TILL POSTSUM                                          
008302*                                                                         
008402*01  -COPY W0005   -PRE  POSTSUM-                                         
008502     EJECT                                                                
008602 01  UT-AREA-START               PIC X(24)   VALUE                        
008702                                 'UT-AREA-START  '.                       
008802                                                                          
008902*01  AREA -COPY W6121E     -PRE UT-                                       
009002     EJECT                                                                
009102 01  UT2-AREA-START              PIC X(24)   VALUE                        
009202                                 'UT2-AREA-START  '.                      
009302                                                                          
009402*01  AREA -COPY PI30INL2   -PRE UT2-                                      
009502     EJECT                                                                
009602*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009702*                                                                         
009802 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009904 01  NYCKLAR-TILL-DLI.                                                    
010004     03  W-IDLEVNRDC-X.                                                   
010104         05  W-IDLEVNRDC         PIC X(5)    VALUE SPACE.                 
010202                                                                          
010302*    --- STATUS-KOD FRÅN IMS                                              
010402 01  STATUS-WS                   PIC XX.                                  
010502     88  SEGMENT-FINNS                       VALUE '  '.                  
010602     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
010702                                                                          
010802 01  GODK-STATUSKODER.                                                    
010902     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011002     EJECT                                                                
011102 01  SSA1                        PIC X(160).                              
011202*    --- IMS FUNKTIONSKODER                                               
011302*01  -COPY W0003                                                          
011402     EJECT                                                                
011502*    ---  DLI INPUT-OUTPUT AREA                                           
011602 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL6'.                        
011702 01  DLI-IO-WDL6.                                                         
011802     03 IO-AREA     PIC X(600) VALUE SPACE.                               
011902         03 DLI-IO-WDL601 REDEFINES IO-AREA.                              
012002*            05 -COPY WDL601                                              
012102     EJECT                                                                
012202         03 DLI-IO-WDL611 REDEFINES IO-AREA.                              
012302*            05 -COPY WDL611                                              
012402     EJECT                                                                
012502 01  DLI-IO-WDB6.                                                         
012602     03 IO-AREA     PIC X(800) VALUE SPACE.                               
012702         03 DLI-IO-WDB601 REDEFINES IO-AREA.                              
012802*            05 -COPY WDB601                                              
012902 LINKAGE SECTION.                                                         
013002                                                                          
013102*01  -COPY W0008  -PRE WDL6-                                              
013202     05  FILLER                  PIC X.                                   
013302     EJECT                                                                
013402*01  -COPY W0008  -PRE WDB6-                                              
013502     05  FILLER                  PIC X.                                   
013602     EJECT                                                                
013702 PROCEDURE DIVISION  USING WDL6-PCB WDB6-PCB.                             
013802 MAIN SECTION.                                                            
013902     ENTRY 'DLITCBL' USING WDL6-PCB WDB6-PCB.                             
014002                                                                          
014102     PERFORM A-INIT                                                       
014202                                                                          
014302     PERFORM IMS-GET-WDL6                                                 
014402     PERFORM UNTIL SEGMENT-SAKNAS                                         
014502       EVALUATE WDL6-SEG-NAME-FB                                          
014602         WHEN 'WDL601'                                                    
014702           MOVE ART-IDARTNR TO SPAR-IDARTNR                               
014801         WHEN 'WDL611'                                                    
014904           PERFORM B-URVAL                                                
015002       END-EVALUATE                                                       
015102       PERFORM IMS-GET-WDL6                                               
015202     END-PERFORM                                                          
015302                                                                          
015402     PERFORM Z-FINIT                                                      
015502     MOVE ZERO TO RETURN-CODE                                             
015602     GOBACK                                                               
015702     .                                                                    
015802     EJECT                                                                
015902 A-INIT SECTION.                                                          
016002                                                                          
016102     OPEN OUTPUT UTFIL                                                    
016103                 UTFIL2                                                   
016202                                                                          
016302     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
016402     MOVE D-AAR    TO DAGENS-DATUM-AAR                                    
016502     MOVE D-VECKA  TO DAGENS-DATUM-VV                                     
016602     MOVE IDPGM    TO POSTSUM-PROGNAMN                                    
016702                                                                          
016802     INITIALIZE WS-IDDC-TABELL                                            
016902     SET WS-IDDC-IX TO +1                                                 
017002     PERFORM IMS-GN-WDB601                                                
017102     PERFORM UNTIL SEGMENT-SAKNAS                                         
017202        MOVE DCS-IDDC        TO TAB-IDDC(WS-IDDC-IX)                      
017302        MOVE DCS-IDLEVNR-DC  TO TAB-IDLEVNR(WS-IDDC-IX)                   
017402        PERFORM IMS-GN-WDB601                                             
017502        SET WS-IDDC-IX UP BY +1                                           
017602        IF WS-IDDC-IX > 100                                               
017702           MOVE 'DC-TABELLEN FULL' TO FELTEXT                             
017802           CALL FELLOG                                                    
017902        END-IF                                                            
018002     END-PERFORM                                                          
018102     .                                                                    
018202     EJECT                                                                
018304 B-URVAL SECTION.                                                         
018402                                                                          
018502     IF INL-IDPTYP = 'R32' AND INL-KDRT = 0                               
018602        IF INL-TIINLINL = ZERO                                            
018702           MOVE 'AAMMDD'     TO DAT-KDDATFORM                             
018802           MOVE INL-TIINLMOT TO DAT-I-TIDATUM                             
018902           CALL WDATKONV USING DAT-KDDATFORM                              
019002                               DAT-I-TIDATUM                              
019102                               DAT-O-TIDATUM                              
019202                               DAT-KDSVAR                                 
019302        ELSE                                                              
019402          MOVE 'AAMMDD'     TO DAT-KDDATFORM                              
019502          MOVE INL-TIINLINL TO DAT-I-TIDATUM                              
019602          CALL WDATKONV USING DAT-KDDATFORM                               
019702                              DAT-I-TIDATUM                               
019802                              DAT-O-TIDATUM                               
019902                               DAT-KDSVAR                                 
020002        END-IF                                                            
020102        IF DAT-KDSVAR-OK                                                  
020202           IF DAT-TIAA = D-AAR AND DAT-TIVV = D-VECKA                     
020302              MOVE SPAR-IDARTNR TO UT-IDARTNR                             
020402              MOVE INL-IDDC     TO UT-IDDC-REC                            
020502              MOVE ZERO         TO UT-KVANTAL                             
020602              IF INL-IDDC-LEV = SPACE                                     
020702                                                                          
020802                 MOVE +1 TO IX                                            
020902                 PERFORM UNTIL IX > TAB-DC-MAX                            
021002                    OR TAB-IDLEVNR(IX) = INL-IDLEVNR                      
021102                    ADD +1 TO IX                                          
021202                 END-PERFORM                                              
021302                 IF IX NOT > TAB-DC-MAX                                   
021402                    MOVE TAB-IDDC(IX) TO UT-IDDC-SEND                     
021502                 END-IF                                                   
021602              ELSE                                                        
021702                 MOVE INL-IDDC-LEV TO UT-IDDC-SEND                        
021802              END-IF                                                      
021902              PERFORM S11-SKRIV-UTFIL                                     
022002              MOVE SPACE TO UT-IDDC-SEND                                  
022104              PERFORM C-URVAL-TILL-VIR                                    
022202           END-IF                                                         
022302        END-IF                                                            
022402     END-IF                                                               
022502     .                                                                    
022602     EJECT                                                                
022702 C-URVAL-TILL-VIR SECTION.                                                
022802                                                                          
022903     MOVE INL-IDDC TO WS-IDDC                                             
023004     IF NDC-US OR NDC-CN                                                  
023104       MOVE INL-IDLEVNR TO W-IDLEVNRDC                                    
024004       PERFORM IMS-GU-WDB601-LEVNR-DC                                     
025004       IF SEGMENT-FINNS                                                   
026004         CONTINUE                                                         
026104       ELSE                                                               
026204*** UNDANTAG FÖR DC 41 OCH 43. PARTNER NO PÅ 4402. ***                    
026304         IF INL-IDLEVNR = 'CUGZQ' OR 'CUGZR'                              
026404            CONTINUE                                                      
026504         ELSE                                                             
026604            MOVE SPAR-IDARTNR TO UT2-PARTNO                               
026704            MOVE INL-IDLEVNR  TO UT2-SUPPNO                               
026804            MOVE INL-KVANTMOT TO UT2-DELQUANT                             
026904           PERFORM S12-SKRIV-UTFIL2                                       
027004         END-IF                                                           
027104       END-IF                                                             
027204     END-IF                                                               
027304     .                                                                    
027404     EJECT                                                                
027504 Z-FINIT SECTION.                                                         
027604                                                                          
027704     CLOSE UTFIL                                                          
027705           UTFIL2                                                         
027804                                                                          
027904     MOVE 'S' TO POSTSUM-OPKOD                                            
028004     CALL POSTSUM USING POSTSUM-PARM                                      
028104     .                                                                    
028204     SKIP3                                                                
028304 S11-SKRIV-UTFIL SECTION.                                                 
028404                                                                          
028504     WRITE UT-POST FROM UT-AREA                                           
028604                                                                          
028704     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
028804     MOVE 'UTFIL'    TO POSTSUM-FDNAMN                                    
028904     MOVE 'W6121ED1' TO POSTSUM-DDNAMN2                                   
029004     CALL POSTSUM USING POSTSUM-PARM                                      
029104     .                                                                    
029204     SKIP3                                                                
029304 S12-SKRIV-UTFIL2 SECTION.                                                
029404                                                                          
029504     WRITE UT2-POST FROM UT2-AREA                                         
029604                                                                          
029704     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
029804     MOVE 'UTFIL2'   TO POSTSUM-FDNAMN                                    
029904     MOVE 'W6121ED2' TO POSTSUM-DDNAMN2                                   
030004     CALL POSTSUM USING POSTSUM-PARM                                      
030104     .                                                                    
030204     EJECT                                                                
030304* --- IMS SEKTIONER ---                                                   
030404                                                                          
030504 IMS-GET-WDL6 SECTION.                                                    
030604     CALL CBLTDLI USING GN WDL6-PCB DLI-IO-WDL6                           
030704     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
030804     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
030904     PERFORM IMS-STATUSKONTROLL                                           
031004     .                                                                    
031104     SKIP3                                                                
031204 IMS-GN-WDB601 SECTION.                                                   
031304     MOVE 'WDB601  ' TO SSA1                                              
031404     MOVE '  GB'     TO GODK-STATUSKODER                                  
031504     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-WDB6 SSA1                      
031604     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
031704     PERFORM IMS-STATUSKONTROLL                                           
031804     .                                                                    
031904     SKIP3                                                                
032004 IMS-GU-WDB601-LEVNR-DC SECTION.                                          
032104     STRING 'WDB601  (IDLEVNDC =' W-IDLEVNRDC ')'                         
032204          DELIMITED BY SIZE INTO SSA1                                     
032304     MOVE '  GE'   TO GODK-STATUSKODER                                    
032404     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB6 SSA1                      
032504     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
032604     PERFORM IMS-STATUSKONTROLL                                           
032605     .                                                                    
032704     SKIP3                                                                
032804 IMS-STATUSKONTROLL SECTION.                                              
032904     SET STATUS-IX TO 1                                                   
033004     SEARCH GODK-STATUS                                                   
033104       AT END                                                             
033204         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
033304           DELIMITED BY SIZE INTO FELTEXT                                 
033404         DISPLAY FELTEXT                                                  
033504         CALL FELLOG                                                      
033604       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
033704         CONTINUE                                                         
034002     END-SEARCH                                                           
040002     .                                                                    
