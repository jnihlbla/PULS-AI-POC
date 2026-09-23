001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     W4756100.                                                
001400 AUTHOR.         OLGRENER CAMELIA.                                        
001500 DATE-WRITTEN.   02/01/23.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800                                                                          
001900*    FUNKTION:                                                            
002210*        PROGRAMMET SKAPAR WDGX4544-SEGMENT PÅ WDR1, UTIFRÅN              
002300*        FIL SOM SKICKAS AV DHL MED ORTSBETECNING (IDCITY) SOM            
002301*                HÖR TILL RESP. POSTNR. INTERVALL.                        
002310*        PROGRAMMET KÖRS ENBART NÄR DHL SKICKAR NY FIL VIA                
002311*        BESTÄLLNINGS RUTIN W475B2.                                       
002320*        FÖRST TAS ALLA GAMLA SEGMENT BORT, SEDAN UTIFRÅN INFILEN         
002330*        LÄGGS UPP DE NYINKOMNA POSTERNA.                                 
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP2                                                                
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003101     SKIP2                                                                
003102*          --- DHLS POSTNR O. ORTBETECKNING                               
003110     SELECT W47561                     ASSIGN TO W47561D1.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003701     SKIP3                                                                
003702 FD  W47561                                                               
003703     RECORDING       F                                                    
003704     BLOCK CONTAINS  0.                                                   
003705                                                                          
003710*01  -COPY W47561       -L.                                               
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100 77  IDPGM                       PIC X(8)    VALUE 'W4756100'.            
004200 01  CHKP-VAR.                                                            
004300     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004400     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004500     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004600     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004700     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004800     03 CHKP-MAX                 PIC S9(3)   VALUE +300 COMP-3.           
004900 77  JA                          PIC X       VALUE 'J'.                   
005000 77  NEJ                         PIC X       VALUE 'N'.                   
005100     SKIP2                                                                
005200 01  FELTEXT.                                                             
005300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005601                                                                          
005602 77  W47561-EOF-SW               PIC X       VALUE 'N'.                   
005610     88  END-OF-W47561                       VALUE 'J'.                   
005900     EJECT                                                                
006000 01  FILLER                      PIC  X(16) VALUE 'DHL-LAND'.             
006100 01  TEST-IDLANDX2               PIC  X(02).                              
006200 01  FILLER REDEFINES TEST-IDLANDX2.                                      
006300*    03    -COPY WWLAND07.                                                
006400     EJECT                                                                
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
007404     SKIP2                                                                
007405                                                                          
007410*01  AREA -COPY W47561      -PRE IN-                                      
007500*                                                                         
007600     EJECT                                                                
007700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007800     SKIP3                                                                
007900 01  NYCKLAR-TILL-DLI.                                                    
008001     03  W-WDGXKEY-X.                                                     
008002         05  W-IDHTYP            PIC X(04)    VALUE '4543'.               
008003         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
008100     SKIP2                                                                
008200*    --- STATUS-KOD FRÅN IMS                                              
008300 01  STATUS-WS                   PIC XX.                                  
008400     88  SEGMENT-FINNS                       VALUE '  '.                  
008500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
008600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
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
010205 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4543'.                    
010206 01  DLI-IO-WDGX4543.                                                     
010210*    03  -COPY WDGX01 -PRE 4543-                                          
010300                                                                          
010700     EJECT                                                                
010710 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4544'.                    
010720 01  DLI-IO-WDGX4544.                                                     
010730*    03  -COPY WDGX4544                                                   
010740                                                                          
010750     EJECT                                                                
010800 LINKAGE SECTION.                                                         
010900                                                                          
011000*01  -COPY W0009   -PRE MSG-                                              
011101                                                                          
011102*01  -COPY W0008  -PRE 4543-                                              
011110     05  FILLER                  PIC X.                                   
011400     EJECT                                                                
011501 PROCEDURE DIVISION  USING MSG-PCB 4543-PCB.                              
011502 MAIN SECTION.                                                            
011510     ENTRY 'DLITCBL' USING MSG-PCB 4543-PCB.                              
011600                                                                          
011800     SKIP2                                                                
011900     PERFORM A-INIT                                                       
012000     PERFORM B-RENSA-4543                                                 
012001                                                                          
012010     PERFORM S01-LAES-W47561                                              
012011                                                                          
012020     IF NOT END-OF-W47561                                                 
012030       MOVE IN-IDLANDX2     TO 4544-IDLANDX2                              
012040       MOVE IN-ADPOSTNR-FOM TO 4544-ADPOSTNR-FOM                          
012050       MOVE IN-ADPOSTNR-TOM TO 4544-ADPOSTNR-TOM                          
012060       MOVE IN-IDCITY       TO 4544-IDCITY                                
012070     END-IF                                                               
012080                                                                          
012090     PERFORM UNTIL END-OF-W47561                                          
012091       MOVE 4544-IDLANDX2 TO TEST-IDLANDX2                                
012092       IF IN-IDCITY = 4544-IDCITY                                         
012094         IF LAND07-DHL-GB                                                 
012095           PERFORM IMS-ISRT-WDGX4544                                      
012096           ADD +1 TO CHKP-ANT                                             
012097           IF CHKP-ANT > CHKP-MAX                                         
012098             PERFORM X-TAG-CHECKPOINT                                     
012099           END-IF                                                         
012100                                                                          
012101           MOVE IN-IDLANDX2     TO 4544-IDLANDX2                          
012102           MOVE IN-ADPOSTNR-FOM TO 4544-ADPOSTNR-FOM                      
012103           MOVE IN-ADPOSTNR-TOM TO 4544-ADPOSTNR-TOM                      
012104           MOVE IN-IDCITY       TO 4544-IDCITY                            
012105         ELSE                                                             
012106           MOVE IN-ADPOSTNR-TOM TO 4544-ADPOSTNR-TOM                      
012107         END-IF                                                           
012108       ELSE                                                               
012109         IF LAND07-DHL                                                    
012112           PERFORM IMS-ISRT-WDGX4544                                      
012113           ADD +1 TO CHKP-ANT                                             
012114           IF CHKP-ANT > CHKP-MAX                                         
012115             PERFORM X-TAG-CHECKPOINT                                     
012116           END-IF                                                         
012117         END-IF                                                           
012118                                                                          
012119         MOVE IN-IDLANDX2     TO 4544-IDLANDX2                            
012120         MOVE IN-ADPOSTNR-FOM TO 4544-ADPOSTNR-FOM                        
012121         MOVE IN-ADPOSTNR-TOM TO 4544-ADPOSTNR-TOM                        
012122         MOVE IN-IDCITY       TO 4544-IDCITY                              
012123       END-IF                                                             
012124                                                                          
012125       PERFORM S01-LAES-W47561                                            
012126     END-PERFORM                                                          
012127                                                                          
012128* ATT INTE GLÖMMA EN EV. STARKARS SISTA POST!                             
012129     IF LAND07-DHL                                                        
012130       PERFORM IMS-ISRT-WDGX4544                                          
012140     END-IF                                                               
013400                                                                          
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
014610     OPEN INPUT W47561                                                    
015200                                                                          
015310     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015600     .                                                                    
015800     EJECT                                                                
015810 B-RENSA-4543 SECTION.                                                    
015820     SKIP2                                                                
015830                                                                          
015840     PERFORM IMS-GHU-WDR101-4543                                          
015850                                                                          
015851     IF SEGMENT-FINNS                                                     
015852* GENOM ATT TA BORT ROTEN RENSAS ALLA BARNSEGM PÅ ENKELT SÄTT             
015853       PERFORM IMS-DLET-WDR101-4543                                       
015854     END-IF                                                               
015855                                                                          
015856* ROTEN LÄGGS TILLBAKA INFÖR UPPLÄGG AV DE NYA BARNSEGM.                  
015857     MOVE W-WDGXKEY-X TO 4543-WDGX01                                      
015858     PERFORM IMS-ISRT-WDR101-4543                                         
015890     .                                                                    
015891     EJECT                                                                
015900 Z-FINIT SECTION.                                                         
016401                                                                          
016410     CLOSE W47561                                                         
016601     SKIP2                                                                
016602     MOVE 'S' TO POSTSUM-OPKOD                                            
016610     CALL POSTSUM USING POSTSUM-PARM                                      
016800     .                                                                    
016901     EJECT                                                                
016902 S01-LAES-W47561  SECTION.                                                
016903     SKIP2                                                                
016904     READ W47561 INTO IN-AREA                                             
016905     AT END                                                               
016907        SET END-OF-W47561 TO TRUE                                         
016908                                                                          
016909     NOT AT END                                                           
016910        MOVE 'W47561'   TO POSTSUM-FDNAMN                                 
016911        MOVE 'W47561D1' TO POSTSUM-DDNAMN2                                
016912        MOVE SPACE      TO POSTSUM-TRANSTYP                               
016913        CALL POSTSUM USING POSTSUM-PARM                                   
016916     END-READ                                                             
016920     .                                                                    
017200     EJECT                                                                
017300 X-TAG-CHECKPOINT   SECTION.                                              
017400                                                                          
018000     PERFORM IMS-CHECKPOINT                                               
018100     MOVE ZERO TO CHKP-ANT                                                
018300     .                                                                    
018400     EJECT                                                                
018500* --- IMS SEKTIONER ---                                                   
018600                                                                          
018702 IMS-GHU-WDR101-4543 SECTION.                                             
018703                                                                          
018704     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-X ')'                         
018705          DELIMITED BY SIZE INTO SSA1                                     
018706     MOVE '  GE' TO GODK-STATUSKODER                                      
018707     CALL CBLTDLI USING GHU 4543-PCB DLI-IO-WDGX4543 SSA1                 
018708     MOVE 4543-STATUS-CODE TO STATUS-WS                                   
018709     PERFORM IMS-STATUSKONTROLL                                           
018710     .                                                                    
018711     SKIP3                                                                
018712 IMS-DLET-WDR101-4543 SECTION.                                            
018713                                                                          
018714     MOVE '  ' TO GODK-STATUSKODER                                        
018715     CALL CBLTDLI USING DLET 4543-PCB DLI-IO-WDGX4543                     
018716     MOVE 4543-STATUS-CODE TO STATUS-WS                                   
018717     PERFORM IMS-STATUSKONTROLL                                           
018718     .                                                                    
018729     SKIP3                                                                
018730 IMS-ISRT-WDR101-4543 SECTION.                                            
018731                                                                          
018732     MOVE 'WDR101 ' TO SSA1                                               
018733     MOVE '  ' TO GODK-STATUSKODER                                        
018734     CALL CBLTDLI USING ISRT 4543-PCB DLI-IO-WDGX4543 SSA1                
018735     MOVE 4543-STATUS-CODE TO STATUS-WS                                   
018736     PERFORM IMS-STATUSKONTROLL                                           
018737     .                                                                    
018738     SKIP3                                                                
018739 IMS-ISRT-WDGX4544 SECTION.                                               
018740                                                                          
018741     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-X ')'                         
018742          DELIMITED BY SIZE INTO SSA1                                     
018743     MOVE 'WDGX4544 ' TO SSA2                                             
018744     MOVE '  II' TO GODK-STATUSKODER                                      
018745     CALL CBLTDLI USING ISRT 4543-PCB DLI-IO-WDGX4544 SSA1 SSA2           
018746     MOVE 4543-STATUS-CODE TO STATUS-WS                                   
018747     PERFORM IMS-STATUSKONTROLL                                           
018748     .                                                                    
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
