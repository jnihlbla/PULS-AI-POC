000101 ID DIVISION.                                                             
000201                                                                          
000301 PROGRAM-ID.     W2133600.                                                
000401 AUTHOR.         PER-ANDERS HELGEGREN.                                    
000501 DATE-WRITTEN.   96/09/27.                                                
000601 DATE-COMPILED.                                                           
000701                                                                          
000801                                                                          
000901*    FUNKTION:                                                            
001001*        LÄSER IN HÄNDELSER PÅ FIL                                        
001101*        OCH UPPDATERAR WDG3 WDG2                                         
001201*        1142  XXAV           *                                           
001301*        2204  XXBJ      *                                                
001401*        2214  XXBI      *                                                
001501*        2301  WDGX2302  *            (GAMLA XXBO)                        
001601*                                                                         
001701*        PROGRAMMET UPPDATERAR WLXXAV (WDG2)                              
001801*        PROGRAMMET UPPDATERAR WLXXBJ (WDG3)                              
001901*        PROGRAMMET UPPDATERAR WLXXBI (WDG3)                              
002001*        PROGRAMMET UPPDATERAR WLXXBO (WDG3) WDGX2302                     
002101*                                                                         
002201*    ÄNDRING:  I SAMBAND MED EVEREST-PROJEKTET ÄNDRAS ÄVEN                
002301*              HTYP 2302 TILL 2301.  (ENL. LASSI)                         
002401*                                                                         
002501*            PSST !                                                       
002601*             (OBS DEN TIDIGARE 2301 HETER NU 2303, DATA PÅ 2304)         
002701*                                                                         
002801*                                                                         
002901*    ABENDKODER:                                                          
003001*        U0016 -  . . . .                                                 
003101*        U1000 -  . . . .                                                 
003201*                                                                         
003301*****************************************************************         
003401* ÄNDRINGAR:                                                              
003501* 2015-12-10  E'TRACKER 10243132 CHINA EXPORT 2015                        
003601*                                                                         
003701*                                                                         
003801*                                                                         
003900                                                                          
004000     EJECT                                                                
004100 ENVIRONMENT DIVISION.                                                    
004200     SKIP2                                                                
004300 INPUT-OUTPUT SECTION.                                                    
004400                                                                          
004500 FILE-CONTROL.                                                            
004600     SKIP2                                                                
004700*          --- HÄNDELSE 1142 2204 2214 2301                               
004800     SELECT W21336                     ASSIGN TO W21336D1.                
004900                                                                          
005000*          --- HÄNDELSE 1142 2204 2214 2301 (REST)                        
005100     SELECT W21337                     ASSIGN TO W21336D2.                
005200     EJECT                                                                
005300 DATA DIVISION.                                                           
005400     SKIP3                                                                
005500 FILE SECTION.                                                            
005600     SKIP3                                                                
005700 FD  W21336                                                               
005800     RECORDING       F                                                    
005900     BLOCK CONTAINS  0.                                                   
006000                                                                          
006100 01  IN-POST             PIC X(14).                                       
006200     SKIP3                                                                
006300 FD  W21337                                                               
006400     RECORDING       F                                                    
006500     BLOCK CONTAINS  0.                                                   
006600                                                                          
006700 01  UT-POST             PIC X(14).                                       
006800     EJECT                                                                
006900 WORKING-STORAGE SECTION.                                                 
007000     SKIP2                                                                
007100                                                                          
007200*    -- CHECKED BY WY2000                                                 
007300 77  IDPGM                       PIC X(8)    VALUE 'W2133600'.            
007400 77  JA                          PIC X       VALUE 'J'.                   
007500 77  NEJ                         PIC X       VALUE 'N'.                   
007600 01  CHKP-ANT                    PIC S9(5) COMP-3.                        
007700     SKIP2                                                                
007800 01  FELTEXT.                                                             
007900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008100                                                                          
008200 77  W21336-EOF-SW               PIC X       VALUE 'N'.                   
008300     88  END-OF-W21336                       VALUE 'J'.                   
008400     SKIP2                                                                
008500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008600 01  FILLER REDEFINES DAGENS-DATUM.                                       
008700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009000     SKIP2                                                                
009100 01  DYNAMISKA-SUBPROGRAM.                                                
009200*                                                                         
009300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009600     EJECT                                                                
009700*    --- PARAMETRAR TILL POSTSUM                                          
009800*                                                                         
009900*01  -COPY W0005   -PRE  POSTSUM-                                         
010000     EJECT                                                                
010100 01  IN-AREA-START               PIC X(24)   VALUE                        
010200                                             'IN-AREA-START'.             
010300     SKIP2                                                                
010400                                                                          
010500 01  IN-AREA.                                                             
010600     03  IN-IDPTYP              PIC X(4).                                 
010700     03  IN-HTYP                PIC X(10).                                
010800     SKIP3                                                                
010900*01  -COPY W2131142  REDEFINES IN-AREA                                    
011000     EJECT                                                                
011100*01  -COPY W2132204  REDEFINES IN-AREA                                    
011200     SKIP3                                                                
011300*01  -COPY W2132213  REDEFINES IN-AREA                                    
011400     EJECT                                                                
011500*01  -COPY W2132302  REDEFINES IN-AREA                                    
011600*                                                                         
011700     EJECT                                                                
011702*01  AREA -COPY W2132204  -PRE 2204-                                      
011703*                                                                         
011710     EJECT                                                                
011800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011900     SKIP3                                                                
012000 01  NYCKLAR-TILL-DLI.                                                    
012100     03  W-WDGXKEY-X-1141.                                                
012200         05  W-IDHTYP-1141       PIC X(04)    VALUE '1141'.               
012300         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
012400     03  W-KDSEGKEY-X.                                                    
012500         05  W-KDSEGKEY          PIC X(1)    VALUE SPACE.                 
012600     03  W-WDG3KEY-X-2203.                                                
012700         05  W-IDHTYP-2203       PIC X(04)    VALUE '2203'.               
012901         05  W-IDDC-2203         PIC X(02)    VALUE SPACE.                
013001         05  FILLER              PIC X(24)    VALUE LOW-VALUE.            
013101     03  W-WDG3KEY-X-2213.                                                
013201         05  W-IDHTYP-2213       PIC X(04)    VALUE '2213'.               
013301         05  W-IDDC-2213         PIC X(02)    VALUE '11'.                 
013401         05  FILLER              PIC X(24)    VALUE LOW-VALUE.            
013501     03  W-WDG3KEY-X-2301.                                                
013601         05  W-IDHTYP-2301       PIC X(04)    VALUE '2301'.               
013701         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
013801     SKIP2                                                                
013901*    --- STATUS-KOD FRÅN IMS                                              
014001 01  STATUS-WS                   PIC XX.                                  
014101     88  SEGMENT-FINNS                       VALUE '  '.                  
014201     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
014301     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014401     88  SEGMENT-SLUT                        VALUE 'GB'.                  
014501     88  IMS-EJ-OK                           VALUE 'XD'.                  
014601     SKIP2                                                                
014701 01  GODK-STATUSKODER.                                                    
014801     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014901     SKIP3                                                                
015001 01  SSA1                        PIC X(64).                               
015101 01  SSA2                        PIC X(64).                               
015201     EJECT                                                                
015301*    --- IMS FUNKTIONSKODER                                               
015401*01  -COPY W0003                                                          
015501     EJECT                                                                
015601*    ---  DLI INPUT-OUTPUT AREA                                           
015701 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
015801     SKIP3                                                                
015901 01  DLI-IO-AREA.                                                         
016001     03  IO-AREA                 PIC X(50)   VALUE SPACE.                 
016101     SKIP3                                                                
016201     03  WLXXAV01 REDEFINES IO-AREA.                                      
016301*        05  -COPY WDGX01  -PRE XXAV-                                     
016401     SKIP3                                                                
016501     03  WLXXAV11 REDEFINES IO-AREA.                                      
016601*        05  -COPY WDGX1142 -PRE XXAV-                                    
016701     EJECT                                                                
016801     03  WLXXBJ01 REDEFINES IO-AREA.                                      
016901*        05  -COPY WDG301  -PRE XXBJ-                                     
017001     SKIP3                                                                
017101     03  WLXXBJ11 REDEFINES IO-AREA.                                      
017201*        05  -COPY WDGX2204 -PRE XXBJ-                                    
017301     EJECT                                                                
017401     03  WLXXBI01 REDEFINES IO-AREA.                                      
017501*        05  -COPY WDG301  -PRE XXBI-                                     
017601     SKIP3                                                                
017701     03  WLXXBI11 REDEFINES IO-AREA.                                      
017801*        05  -COPY WDGX2214 -PRE XXBI-                                    
017901     EJECT                                                                
018001     03  WDG301   REDEFINES IO-AREA.                                      
018101*                                     GAMLA XXBO01                        
018201*        05  -COPY WDG301  -PRE 2301-                                     
018301     SKIP3                                                                
018401     03  WDGX2302 REDEFINES IO-AREA.                                      
018501*                                     GAMLA XXBO11                        
018601*        05  -COPY WDGX2302 -PRE 2301-                                    
018701     EJECT                                                                
018801 LINKAGE SECTION.                                                         
018901                                                                          
019001*01  -COPY W0009   -PRE MSG-                                              
019101     EJECT                                                                
019201*01  -COPY W0008  -PRE XXAV-                                              
019301     05  FILLER                  PIC X.                                   
019401     EJECT                                                                
019501*01  -COPY W0008  -PRE XXBJ-                                              
019601     05  FILLER                  PIC X.                                   
019701     EJECT                                                                
019801*01  -COPY W0008  -PRE XXBI-                                              
019901     05  FILLER                  PIC X.                                   
020001     EJECT                                                                
020101*01  -COPY W0008  -PRE 2301-                                              
020201     05  FILLER                  PIC X.                                   
020301     EJECT                                                                
020401 PROCEDURE DIVISION  USING MSG-PCB XXAV-PCB XXBJ-PCB                      
020501     XXBI-PCB 2301-PCB.                                                   
020601 MAIN SECTION.                                                            
020701     ENTRY 'DLITCBL' USING MSG-PCB XXAV-PCB XXBJ-PCB                      
020801     XXBI-PCB 2301-PCB.                                                   
020901                                                                          
021001                                                                          
021101     PERFORM A-INIT                                                       
021201     MOVE +1 TO CHKP-ANT                                                  
021301     PERFORM S01-LAES-W21336                                              
021401     PERFORM UNTIL END-OF-W21336 OR CHKP-ANT > 500                        
021501        MOVE IN-HTYP TO IO-AREA                                           
021601* OBS. ATT IN-HTYP INNEHÅLLER HELA DB-SEGM SOM ISRT:AS NEDAN              
021701        IF IN-IDPTYP = '1142'                                             
021801           PERFORM IMS-ISRT-XXAV-1142                                     
021901           ADD +1 TO CHKP-ANT                                             
022001        END-IF                                                            
022101        IF IN-IDPTYP = '2204'                                             
022202           MOVE IN-AREA    TO 2204-AREA                                   
022204           MOVE 2204-IDDC  TO W-IDDC-2203                                 
022301           PERFORM IMS-ISRT-XXBJ-2204                                     
022401           ADD +1 TO CHKP-ANT                                             
022501        END-IF                                                            
022601        IF IN-IDPTYP = '2213' OR '2214'                                   
022701           PERFORM IMS-ISRT-XXBI-2214                                     
022801           ADD +1 TO CHKP-ANT                                             
022901        END-IF                                                            
023001        IF IN-IDPTYP = '2301'                                             
023101           PERFORM IMS-ISRT-2301-2302                                     
023201           ADD +1 TO CHKP-ANT                                             
023301        END-IF                                                            
023401        PERFORM S01-LAES-W21336                                           
023501     END-PERFORM                                                          
023601                                                                          
023701     IF W21336-EOF-SW = NEJ                                               
023801        PERFORM UNTIL END-OF-W21336                                       
023901          PERFORM S11-SKRIV-W21337                                        
024001          PERFORM S01-LAES-W21336                                         
024101        END-PERFORM                                                       
024201     END-IF                                                               
024301                                                                          
024401     PERFORM Z-FINIT                                                      
024501                                                                          
024601     MOVE ZERO TO RETURN-CODE                                             
024701     GOBACK                                                               
024801     .                                                                    
024901     EJECT                                                                
025001 A-INIT SECTION.                                                          
025101     SKIP2                                                                
025201                                                                          
025301     OPEN INPUT W21336                                                    
025401         OUTPUT W21337                                                    
025501                                                                          
025601                                                                          
025701     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
025801     .                                                                    
025901     EJECT                                                                
026001 Z-FINIT SECTION.                                                         
026101                                                                          
026201                                                                          
026301     CLOSE W21336                                                         
026401           W21337                                                         
026501     SKIP2                                                                
026601     MOVE 'S' TO POSTSUM-OPKOD                                            
026701     CALL POSTSUM USING POSTSUM-PARM                                      
026801     .                                                                    
026901     EJECT                                                                
027001 S01-LAES-W21336  SECTION.                                                
027101     SKIP2                                                                
027201     READ W21336 INTO IN-AREA                                             
027301     AT END                                                               
027401        SET END-OF-W21336 TO TRUE                                         
027501                                                                          
027601     NOT AT END                                                           
027701        MOVE 'W21336' TO POSTSUM-FDNAMN                                   
027801        MOVE 'W21336D1' TO POSTSUM-DDNAMN2                                
027901        MOVE IN-IDPTYP TO POSTSUM-TRANSTYP                                
028001        CALL POSTSUM USING POSTSUM-PARM                                   
028101     END-READ                                                             
028201     .                                                                    
028301     EJECT                                                                
028401 S11-SKRIV-W21337  SECTION.                                               
028501     SKIP2                                                                
028601     WRITE UT-POST FROM IN-AREA                                           
028701                                                                          
028801        MOVE 'W21337' TO POSTSUM-FDNAMN                                   
028901        MOVE 'W21336D2' TO POSTSUM-DDNAMN2                                
029001        MOVE IN-IDPTYP TO POSTSUM-TRANSTYP                                
029101        CALL POSTSUM USING POSTSUM-PARM                                   
029201     .                                                                    
029301     EJECT                                                                
029401* --- IMS SEKTIONER ---                                                   
029501     SKIP3                                                                
029601 IMS-ISRT-XXAV-1142 SECTION.                                              
029701                                                                          
029801     STRING 'WLXXAV01(WDGXKEY  =' W-WDGXKEY-X-1141 ')'                    
029901          DELIMITED BY SIZE INTO SSA1                                     
030001     MOVE 'WLXXAV11 ' TO SSA2                                             
030101     MOVE '  II' TO GODK-STATUSKODER                                      
030201     CALL CBLTDLI USING ISRT XXAV-PCB DLI-IO-AREA SSA1 SSA2               
030301     MOVE XXAV-STATUS-CODE TO STATUS-WS                                   
030401     PERFORM IMS-STATUSKONTROLL                                           
030501                                                                          
030601        MOVE 'WDG2  '   TO POSTSUM-FDNAMN                                 
030701        MOVE 'XXAV11  ' TO POSTSUM-DDNAMN2                                
030801        MOVE '1142'     TO POSTSUM-TRANSTYP                               
030901        CALL POSTSUM USING POSTSUM-PARM                                   
031001     .                                                                    
031101     EJECT                                                                
031201 IMS-ISRT-XXBJ-2204 SECTION.                                              
031301                                                                          
031401     STRING 'WLXXBJ01(WDG3KEY  =' W-WDG3KEY-X-2203 ')'                    
031501          DELIMITED BY SIZE INTO SSA1                                     
031601     MOVE 'WLXXBJ11 ' TO SSA2                                             
031701     MOVE '  II' TO GODK-STATUSKODER                                      
031801     CALL CBLTDLI USING ISRT XXBJ-PCB DLI-IO-AREA SSA1 SSA2               
031901     MOVE XXBJ-STATUS-CODE TO STATUS-WS                                   
032001     PERFORM IMS-STATUSKONTROLL                                           
032101                                                                          
032201        MOVE 'WDG3  '   TO POSTSUM-FDNAMN                                 
032301        MOVE 'XXBJ11  ' TO POSTSUM-DDNAMN2                                
032401        MOVE '2204'     TO POSTSUM-TRANSTYP                               
032501        CALL POSTSUM USING POSTSUM-PARM                                   
032601     .                                                                    
032701     EJECT                                                                
032801 IMS-ISRT-XXBI-2214 SECTION.                                              
032901                                                                          
033001     STRING 'WLXXBI01(WDG3KEY  =' W-WDG3KEY-X-2213 ')'                    
033101          DELIMITED BY SIZE INTO SSA1                                     
033201     MOVE 'WLXXBI11 ' TO SSA2                                             
033301     MOVE '  II' TO GODK-STATUSKODER                                      
033401     CALL CBLTDLI USING ISRT XXBI-PCB DLI-IO-AREA SSA1 SSA2               
033501     MOVE XXBI-STATUS-CODE TO STATUS-WS                                   
033601     PERFORM IMS-STATUSKONTROLL                                           
033701                                                                          
033801        MOVE 'WDG3  '   TO POSTSUM-FDNAMN                                 
033901        MOVE 'XXBI11  ' TO POSTSUM-DDNAMN2                                
034001        MOVE '2214'     TO POSTSUM-TRANSTYP                               
034101        CALL POSTSUM USING POSTSUM-PARM                                   
034201     .                                                                    
034301     EJECT                                                                
034401 IMS-ISRT-2301-2302 SECTION.                                              
034501                                                                          
034601     STRING 'WDG301  (WDG3KEY  =' W-WDG3KEY-X-2301 ')'                    
034701          DELIMITED BY SIZE INTO SSA1                                     
034801     MOVE 'WDGX2302 ' TO SSA2                                             
034901     MOVE '  II' TO GODK-STATUSKODER                                      
035001     CALL CBLTDLI USING ISRT 2301-PCB DLI-IO-AREA SSA1 SSA2               
035101     MOVE 2301-STATUS-CODE TO STATUS-WS                                   
035201     PERFORM IMS-STATUSKONTROLL                                           
035301                                                                          
035401        MOVE 'WDG3  '   TO POSTSUM-FDNAMN                                 
035501        MOVE 'WDGX2302' TO POSTSUM-DDNAMN2                                
035601        MOVE '2301'     TO POSTSUM-TRANSTYP                               
035701        CALL POSTSUM USING POSTSUM-PARM                                   
035801     .                                                                    
035901     EJECT                                                                
036001 IMS-STATUSKONTROLL SECTION.                                              
036101     SKIP2                                                                
036201     SET STATUS-IX TO 1                                                   
036301     SEARCH GODK-STATUS                                                   
036401       AT END                                                             
036501         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
036601           DELIMITED BY SIZE INTO FELTEXT                                 
036701         DISPLAY FELTEXT                                                  
036801         CALL FELLOG                                                      
036901       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
037001         CONTINUE                                                         
037101     END-SEARCH                                                           
038001     .                                                                    
