001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W4764600.                                                
001300 AUTHOR.         CAMELIA OLGRENER.                                        
001400 DATE-WRITTEN.   07/04/24.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001800*    FUNKTION:                                                            
001900*        PROGRAMMET LÄSER IN FILER MED POSTER SOM SKALL RENSAS PÅ         
002000*        WDR4. RENSNINGSFILEN KOMMER FRÅN PGM W4764500.                   
002100*                                                                         
002210*        PROGRAMMET UPPDATERAR WDR4                                       
002300*                                                                         
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP2                                                                
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003101     SKIP2                                                                
003102*          --- RENSNINGSPOSTER FÖR WDGX4545                               
003110     SELECT W47645C                    ASSIGN TO W47646D1.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003701     SKIP3                                                                
003702 FD  W47645C                                                              
003703     RECORDING       F                                                    
003704     BLOCK CONTAINS  0.                                                   
003705                                                                          
003710*01  -COPY WDGX4546    -L.                                                
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004001                                                                          
004010*    -- CHECKED BY WY2000                                                 
004100 77  IDPGM                       PIC X(8)    VALUE 'W4764600'.            
004140 77  JA                          PIC X       VALUE 'J'.                   
004150 77  NEJ                         PIC X       VALUE 'N'.                   
004160 77  W-ANTAL-4546                PIC 9(4)    VALUE ZERO.                  
004200 01  CHKP-VAR.                                                            
004300     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004400     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004500     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004600     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004700     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004801     03 CHKP-MAX                 PIC S9(3)   VALUE +300 COMP-3.           
005020                                                                          
005021 01  FELTEXT.                                                             
005022     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005023     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005024                                                                          
005602 77  W47645C-EOF-SW              PIC X       VALUE 'N'.                   
005610     88  END-OF-W47645C                      VALUE 'J'.                   
005900     EJECT                                                                
006600 01  DYNAMISKA-SUBPROGRAM.                                                
006700*                                                                         
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007010     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007101     EJECT                                                                
007102*    --- PARAMETRAR TILL POSTSUM                                          
007103*                                                                         
007110*01  -COPY W0005   -PRE  POSTSUM-                                         
007401     EJECT                                                                
007402 01  IN-AREA-START               PIC X(24)   VALUE                        
007403                                             'IN-AREA-START'.             
007410*01  AREA -COPY WDGX4546   -PRE IN-                                       
007600     EJECT                                                                
007700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007800     SKIP3                                                                
007900 01  NYCKLAR-TILL-DLI.                                                    
008000                                                                          
008010     03 W-WDGXKEY-X.                                                      
008012       05 W-IDHTYP               PIC X(4)    VALUE '4545'.                
008013       05 FILLER                 PIC X(26)   VALUE LOW-VALUE.             
008019                                                                          
008080     03 W-KY4546-X.                                                       
008090       05 W-IDPRODNR             PIC S9(7)   VALUE ZERO COMP-3.           
008092       05 W-IDKOLLI              PIC S9(5)   VALUE ZERO COMP-3.           
008100     EJECT                                                                
008200*    --- STATUS-KOD FRÅN IMS                                              
008300 01  STATUS-WS                   PIC XX.                                  
008400     88  SEGMENT-FINNS                       VALUE '  '.                  
008600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008800     88  IMS-EJ-OK                           VALUE 'XD'.                  
008900     SKIP2                                                                
009000 01  GODK-STATUSKODER.                                                    
009100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009200     SKIP3                                                                
009300 01  SSA1                        PIC X(64).                               
009400 01  SSA2                        PIC X(64).                               
009500     EJECT                                                                
009600*    --- IMS FUNKTIONSKODER                                               
009700*01  -COPY W0003                                                          
009800     EJECT                                                                
010000*    ---  DLI INPUT-OUTPUT AREA                                           
010100                                                                          
010205 01  FILLER         PIC X(16) VALUE 'DLI-IO-AREA'.                        
010220 01  DLI-IO-AREA.                                                         
010251*    03  -COPY WDGX4546                                                   
010700     EJECT                                                                
010800 LINKAGE SECTION.                                                         
010900                                                                          
011000*01  -COPY W0009  -PRE MSG-                                               
011101                                                                          
011102*01  -COPY W0008  -PRE WDR4-                                              
011110     05  FILLER                  PIC X.                                   
011400     EJECT                                                                
011501 PROCEDURE DIVISION  USING MSG-PCB WDR4-PCB.                              
011502 MAIN SECTION.                                                            
011510     ENTRY 'DLITCBL' USING MSG-PCB WDR4-PCB.                              
011600                                                                          
011800     SKIP2                                                                
011900     PERFORM A-INIT                                                       
011910                                                                          
012000     PERFORM B-RENSA-4546                                                 
013440                                                                          
013500     PERFORM Z-FINIT                                                      
013600                                                                          
013700     MOVE ZERO TO RETURN-CODE                                             
013800     GOBACK                                                               
013900     .                                                                    
014000     EJECT                                                                
014100 A-INIT SECTION.                                                          
014200     SKIP2                                                                
014300                                                                          
014400     PERFORM IMS-RESTART                                                  
014601                                                                          
014610     OPEN INPUT W47645C                                                   
014900                                                                          
015200     MOVE +0    TO CHKP-ANT                                               
015310     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015600     .                                                                    
015800     EJECT                                                                
015810 B-RENSA-4546 SECTION.                                                    
015811                                                                          
015820     PERFORM S01-LAES-W47645C                                             
015830     PERFORM UNTIL END-OF-W47645C                                         
015838                                                                          
015840       IF CHKP-ANT > CHKP-MAX                                             
015851         PERFORM IMS-CHECKPOINT                                           
015852         MOVE ZERO TO CHKP-ANT                                            
015860       END-IF                                                             
015870                                                                          
015895       MOVE IN-4546-IDPRODNR TO W-IDPRODNR                                
015896       MOVE IN-4546-IDKOLLI  TO W-IDKOLLI                                 
015897       PERFORM IMS-GHU-WDGX4546                                           
015898       IF SEGMENT-FINNS                                                   
015899         PERFORM IMS-DLET-WDGX4546                                        
015900         ADD +1 TO CHKP-ANT                                               
015901                   W-ANTAL-4546                                           
             ELSE                                                               
               DISPLAY 'SAKNAS? = ' IN-4546-IDPRODNR                            
                                    IN-4546-IDKOLLI                             
015902       END-IF                                                             
015903                                                                          
015904       PERFORM S01-LAES-W47645C                                           
015905     END-PERFORM                                                          
015906     .                                                                    
015907     EJECT                                                                
015981                                                                          
015990 Z-FINIT SECTION.                                                         
016401                                                                          
016410     CLOSE W47645C                                                        
016431                                                                          
016440     DISPLAY 'ANTAL 4546-BORTTAG: ' W-ANTAL-4546                          
016601                                                                          
016602     MOVE 'S' TO POSTSUM-OPKOD                                            
016610     CALL POSTSUM USING POSTSUM-PARM                                      
016800     .                                                                    
016901     EJECT                                                                
016902 S01-LAES-W47645C SECTION.                                                
016903     SKIP2                                                                
016904     READ W47645C INTO IN-AREA                                            
016905     AT END                                                               
016907        SET END-OF-W47645C TO TRUE                                        
016908                                                                          
016909     NOT AT END                                                           
016910        MOVE 'W47645C'   TO POSTSUM-FDNAMN                                
016911        MOVE 'W47646D1'  TO POSTSUM-DDNAMN2                               
016912        MOVE '4546'      TO POSTSUM-TRANSTYP                              
016913        CALL POSTSUM USING POSTSUM-PARM                                   
016916     END-READ                                                             
016920     .                                                                    
017200     EJECT                                                                
018500* --- IMS SEKTIONER ---                                                   
018600                                                                          
018744 IMS-GHU-WDGX4546 SECTION.                                                
018745                                                                          
018724     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-X ')'                         
018725          DELIMITED BY SIZE INTO SSA1                                     
018748     STRING 'WDGX4546(KY4546   =' W-KY4546-X ')'                          
018749          DELIMITED BY SIZE INTO SSA2                                     
018750     MOVE '  GE' TO GODK-STATUSKODER                                      
018751     CALL CBLTDLI USING GHU WDR4-PCB DLI-IO-AREA SSA1 SSA2                
018752     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
018753     PERFORM IMS-STATUSKONTROLL                                           
018754     .                                                                    
018755     SKIP3                                                                
018766 IMS-DLET-WDGX4546 SECTION.                                               
018767                                                                          
018768     MOVE '  ' TO GODK-STATUSKODER                                        
018769     CALL CBLTDLI USING DLET WDR4-PCB DLI-IO-AREA                         
018770     MOVE WDR4-STATUS-CODE TO STATUS-WS                                   
018771     PERFORM IMS-STATUSKONTROLL                                           
018780     .                                                                    
018800     EJECT                                                                
018900 IMS-RESTART SECTION.                                                     
019000     SKIP2                                                                
019100     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
019200     MOVE '  ' TO GODK-STATUSKODER                                        
019300     CALL CBLTDLI USING XRST MSG-PCB                                      
019400                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
019500                        CHKP-AREA-LENGTH CHKP-AREA                        
019600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
019700     PERFORM IMS-STATUSKONTROLL                                           
019800     .                                                                    
019900     SKIP3                                                                
020000 IMS-CHECKPOINT SECTION.                                                  
020100     SKIP2                                                                
020200     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
020300     MOVE '  XD' TO GODK-STATUSKODER                                      
020400     CALL CBLTDLI USING CHKP MSG-PCB                                      
020500                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
020600                        CHKP-AREA-LENGTH CHKP-AREA                        
020700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
020800     PERFORM IMS-STATUSKONTROLL                                           
020900                                                                          
021000     IF IMS-EJ-OK                                                         
021100       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
021200       DISPLAY FELTEXT                                                    
021300       CALL FELLOG                                                        
021400     END-IF                                                               
021500     .                                                                    
021600     EJECT                                                                
021700 IMS-STATUSKONTROLL SECTION.                                              
021800     SKIP2                                                                
021900     SET STATUS-IX TO 1                                                   
022000     SEARCH GODK-STATUS                                                   
022100       AT END                                                             
022200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
022300           DELIMITED BY SIZE INTO FELTEXT                                 
022400         DISPLAY FELTEXT                                                  
022500         CALL FELLOG                                                      
022600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
022700         CONTINUE                                                         
022800     END-SEARCH                                                           
022900     .                                                                    
