000100 ID DIVISION.                                                             
000201 PROGRAM-ID.     W2241500.                                                
000301 AUTHOR.         KJELLSON GÖRAN.                                          
000401 DATE-WRITTEN.   13/02/18.                                                
000501 DATE-COMPILED.                                                           
000601                                                                          
000701                                                                          
000801*    FUNKTION:                                                            
000901*        UPPDATERING OMSPEC LEVERANSPLAN                                  
001001*        FIL W22415 FRÅN W2241400                                         
001101*                                                                         
001201*        PROGRAMMET UPPDATERAR WDK7                                       
001301*        PROGRAMMET UPPDATERAR WDD9                                       
001401*        PROGRAMMET UPPDATERAR WDD6                                       
001501*        PROGRAMMET UPPDATERAR WDR3                                       
001601*                                                                         
001701*                                                                         
001801****---------------------------------------------------------             
001901*--- PROGRAMÄNDRINGAR                                                     
002001****---------------------------------------------------------             
002101*                                                                         
002201* 2014-05-07  E'TRACKER 10230472  RÄTTNING AV BUGG VID                    
002301*                                 HELGDAGSJUSTERING.LÄGGER UPP            
002401*                                 DUBLETTER AV WDD905 VID                 
002501*                                 HELGFLYTT AV AVROP.                     
002601*                                                                         
002602* 2015-09-11  E'TRACKER 10130993                                          
002603*             REDUCE NUMBER OF DELIVERY SCHEDULES                         
002604*                                                                         
002605*                                                                         
002606*                                                                         
002701                                                                          
002801                                                                          
002901 ENVIRONMENT DIVISION.                                                    
003001 INPUT-OUTPUT SECTION.                                                    
003101                                                                          
003201 FILE-CONTROL.                                                            
003301                                                                          
003401*          --- UPPDATERINGSPOSTER                                         
003501     SELECT W22415                     ASSIGN TO W22415D1.                
003601                                                                          
003701                                                                          
003801 DATA DIVISION.                                                           
003901 FILE SECTION.                                                            
004001                                                                          
004101 FD  W22415                                                               
004201     RECORDING       F                                                    
004301     BLOCK CONTAINS  0.                                                   
004401                                                                          
004501*01  -COPY W22415      -L.                                                
004601                                                                          
004701                                                                          
004801 WORKING-STORAGE SECTION.                                                 
004901                                                                          
005001 77  IDPGM                       PIC X(8)    VALUE 'W2241500'.            
005101 77  JA                          PIC X       VALUE 'J'.                   
005201 77  NEJ                         PIC X       VALUE 'N'.                   
005301                                                                          
005401 01  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
005501 01  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
005601                                                                          
005701 01  WC-IDPTYP.                                                           
005801     03  BORTTAG-OMSPEC          PIC X(3)    VALUE '001'.                 
005901     03  BORTTAG-FORSLAG         PIC X(3)    VALUE '002'.                 
006001     03  UPDATE-WDK722           PIC X(3)    VALUE '003'.                 
006101     03  NYUPPL-OMSPEC           PIC X(3)    VALUE '004'.                 
006201     03  UPPDAT-OMSPEC           PIC X(3)    VALUE '005'.                 
006301     03  NYUPPL-LEV              PIC X(3)    VALUE '006'.                 
006401     03  UPD-AVROP               PIC X(3)    VALUE '007'.                 
006501     03  NYUPPL-AVROP            PIC X(3)    VALUE '008'.                 
006601                                                                          
006701 01  FELTEXT.                                                             
006801     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006901     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007001                                                                          
007101 77  W-ANTAL-POSTER              PIC 9(7)    VALUE ZERO.                  
007201                                                                          
007301 77  W22415-EOF-SW               PIC X       VALUE 'N'.                   
007401     88  END-OF-W22415                       VALUE 'J'.                   
007501                                                                          
007601 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007701 01  FILLER REDEFINES DAGENS-DATUM.                                       
007801     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007901     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008001     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008101                                                                          
008201 01  DYNAMISKA-SUBPROGRAM.                                                
008301*                                                                         
008401     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008501     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008601     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008701                                                                          
008801*    --- PARAMETRAR TILL POSTSUM                                          
008901*                                                                         
009001*01  -COPY W0005   -PRE  POSTSUM-                                         
009101                                                                          
009201 01  UPLP-AREA-START             PIC X(24)   VALUE                        
009301                                             'UPLP-AREA-START'.           
009401                                                                          
009501*01  UPLP-AREA -COPY W22415                                               
009601*                                                                         
009701                                                                          
009801                                                                          
009901 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010001                                                                          
010101 01  CHKP-VAR.                                                            
010201 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
010301 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
010401 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
010501 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
010601 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
010701 03  CHKP-MAX                    PIC S9(3)   VALUE +500.                  
010801                                                                          
010901 01  NYCKLAR-TILL-DLI.                                                    
011001     03  W-IDARTNR-X.                                                     
011101         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011201     03  W-IDDC-X.                                                        
011301         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
011401                                                                          
011501     03  W-WDD901KY-X.                                                    
011601         05 W-IDARTNR-D9         PIC S9(9)   VALUE ZERO COMP-3.           
011701         05 W-IDDC-D9            PIC X(2)    VALUE SPACE.                 
011801                                                                          
011901     03  W-IDLEVNR-X.                                                     
012001         05 W-IDLEVNR            PIC  X(5)   VALUE SPACE.                 
012101                                                                          
012201     03  W-KDAVROP-X.                                                     
012301         05 W-KDAVROP            PIC S9      VALUE ZERO COMP-3.           
012401                                                                          
012501     03  W-DAXLEVSP-X.                                                    
012601         05 W-DAXLEVSP           PIC  9(6)   VALUE ZERO.                  
012701                                                                          
012801     03  W-DASPECST-X.                                                    
012901         05 W-DASPECST           PIC  9(6)   VALUE ZERO.                  
013001                                                                          
013101     03  W-WDD905KY-X.                                                    
013201         05 W-DAAVROP-AVS        PIC  9(6)   VALUE ZERO.                  
013301         05 W-TILEVDAG           PIC S9      VALUE ZERO COMP-3.           
013401                                                                          
013501     03  W-WDD601KY-X.                                                    
013601         05 W-IDDC-D6            PIC  X(2)   VALUE SPACE.                 
013701         05 W-IDLEVNR-D6         PIC  X(5)   VALUE SPACE.                 
013801         05 W-IDARTNR-D6         PIC S9(9)   VALUE ZERO COMP-3.           
013901         05 W-IDANSK-D6          PIC S9(3)   VALUE ZERO COMP-3.           
014001                                                                          
014101     03  W-WDGXKEY-4579-X.                                                
014201          05 W-IDHTYP-4579       PIC X(4)    VALUE '4579'.                
014301          05 W-IDPGM             PIC X(8)    VALUE 'W2241500'.            
014401          05 FILLER              PIC X(18)   VALUE LOW-VALUE.             
014501                                                                          
014601                                                                          
014701                                                                          
014801*    --- STATUS-KOD FRÅN IMS                                              
014901 01  STATUS-WS                   PIC XX.                                  
015001     88  SEGMENT-FINNS                       VALUE '  '.                  
015101     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
015201     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015301     88  SEGMENT-SLUT                        VALUE 'GB'.                  
015401     88  IMS-EJ-OK                           VALUE 'XD'.                  
015501                                                                          
015601 01  GODK-STATUSKODER.                                                    
015701     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015801                                                                          
015901 01  ALL-SSA.                                                             
016001     03 SSA1                     PIC X(64).                               
016101     03 SSA2                     PIC X(64).                               
016201     03 SSA3                     PIC X(64).                               
016301                                                                          
016401                                                                          
016501*    --- IMS FUNKTIONSKODER                                               
016601*01  -COPY W0003                                                          
016701                                                                          
016801*    ---  DLI INPUT-OUTPUT AREA                                           
016901                                                                          
017001 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
017101 01  DLI-IO-WDK711.                                                       
017201*    03  -COPY WDK711                                                     
017301                                                                          
017401 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK722'.                      
017501 01  DLI-IO-WDK722.                                                       
017601*    03  -COPY WDK722                                                     
017701                                                                          
017801                                                                          
017901 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD901'.                      
018001 01  DLI-IO-WDD901.                                                       
018101*    03  -COPY WDD901  -PRE D901-                                         
018201                                                                          
018301 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD902'.                      
018401 01  DLI-IO-WDD902.                                                       
018501*    03  -COPY WDD902  -PRE D902-                                         
018601                                                                          
019101 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD904'.                      
019201 01  DLI-IO-WDD904.                                                       
019301*    03  -COPY WDD904  -PRE D904-                                         
019401                                                                          
019501 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD905'.                      
019601 01  DLI-IO-WDD905.                                                       
019701*    03  -COPY WDD905  -PRE D905-                                         
019801                                                                          
019901                                                                          
020001 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD601'.                      
020101 01  DLI-IO-WDD601.                                                       
020201*    03  -COPY WDD601  -PRE D901-                                         
020301                                                                          
020401                                                                          
020501 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4580'.                    
020601 01  DLI-IO-WDGX4580.                                                     
020701*    03  -COPY WDGX4580                                                   
020801                                                                          
020901                                                                          
021001 LINKAGE SECTION.                                                         
021101                                                                          
021201*01  -COPY W0009   -PRE MSG-                                              
021301                                                                          
021401*01  -COPY W0008  -PRE WDK7-                                              
021501     05  FILLER                  PIC X.                                   
021601                                                                          
021701*01  -COPY W0008  -PRE WDD9-                                              
021801     05  FILLER                  PIC X.                                   
021901                                                                          
022001*01  -COPY W0008  -PRE WDD6-                                              
022101     05  FILLER                  PIC X.                                   
022201                                                                          
022301*01  -COPY W0008  -PRE 4579-                                              
022401     05  FILLER                  PIC X.                                   
022501                                                                          
022601                                                                          
022701 PROCEDURE DIVISION  USING MSG-PCB WDK7-PCB WDD9-PCB WDD6-PCB             
022801                                   4579-PCB.                              
022901 MAIN SECTION.                                                            
023001     ENTRY 'DLITCBL' USING MSG-PCB WDK7-PCB WDD9-PCB WDD6-PCB             
023101                                   4579-PCB.                              
023201                                                                          
023301     PERFORM A-INIT                                                       
023401     PERFORM S01-LAES-W22415                                              
023501     PERFORM UNTIL END-OF-W22415                                          
023601       EVALUATE UPLP-IDPTYP                                               
023701          WHEN BORTTAG-OMSPEC                                             
023801               PERFORM B-BORTTAG-OMSPEC                                   
023901                                                                          
024001          WHEN BORTTAG-FORSLAG                                            
024101               PERFORM C-BORTTAG-FORSLAG                                  
024201                                                                          
024301          WHEN UPDATE-WDK722                                              
024401               PERFORM D-UPDATE-WDK722                                    
024501                                                                          
024601          WHEN NYUPPL-OMSPEC                                              
024701               PERFORM E-NYUPPL-OMSPEC                                    
024801                                                                          
024901          WHEN UPPDAT-OMSPEC                                              
025001               PERFORM F-UPPDAT-OMSPEC                                    
025101                                                                          
025201          WHEN NYUPPL-LEV                                                 
025301               PERFORM G-NYUPPL-LEV                                       
025401                                                                          
025501          WHEN UPD-AVROP                                                  
025601               PERFORM H-UPD-AVROP                                        
025701                                                                          
025801          WHEN NYUPPL-AVROP                                               
025901               PERFORM I-NYUPPL-AVROP                                     
026001                                                                          
026101       END-EVALUATE                                                       
026201                                                                          
026301       IF CHKP-ANT > CHKP-MAX                                             
026401          PERFORM X-TAG-CHECKPOINT                                        
026501       END-IF                                                             
026601       PERFORM S01-LAES-W22415                                            
026701     END-PERFORM                                                          
026801                                                                          
026901                                                                          
027001     PERFORM Z-FINIT                                                      
027101                                                                          
027201     MOVE ZERO TO RETURN-CODE                                             
027301     GOBACK                                                               
027401     .                                                                    
027501                                                                          
027601                                                                          
027701 A-INIT SECTION.                                                          
027801     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
027901                                                                          
028001     OPEN INPUT W22415                                                    
028101                                                                          
028201     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
028301                                                                          
028401     PERFORM IMS-RESTART                                                  
028501                                                                          
028601     PERFORM IMS-GHU-RESTART                                              
028701     IF 4580-KVPOST > ZERO                                                
028801        MOVE ZERO TO W-ANTAL-POSTER                                       
028901        PERFORM UNTIL W-ANTAL-POSTER = 4580-KVPOST                        
029001           PERFORM S01-LAES-W22415                                        
029101           ADD 1  TO W-ANTAL-POSTER                                       
029201        END-PERFORM                                                       
029301     END-IF                                                               
029401     .                                                                    
029501                                                                          
029601                                                                          
029701 B-BORTTAG-OMSPEC SECTION.                                                
029801     MOVE 'B-BORTTAG-OMSPEC' TO CURRENT-SECTION                           
029901                                                                          
030001     MOVE UPLP-IDARTNR       TO W-IDARTNR-D9                              
030101     MOVE UPLP-IDDC          TO W-IDDC-D9                                 
030201     PERFORM IMS-GU-WDD901                                                
030301     IF SEGMENT-FINNS                                                     
030401                                                                          
030501        PERFORM IMS-GHNP-WDD904                                           
030601        PERFORM UNTIL SEGMENT-SAKNAS                                      
030701           PERFORM IMS-DLET-WDD904                                        
030801           PERFORM IMS-GHNP-WDD904                                        
030901        END-PERFORM                                                       
031001     END-IF                                                               
031101     .                                                                    
031201                                                                          
031301                                                                          
031401 C-BORTTAG-FORSLAG SECTION.                                               
031501     MOVE 'C-BORTTAG-FORS  ' TO CURRENT-SECTION                           
031601                                                                          
031701     MOVE UPLP-IDARTNR       TO W-IDARTNR-D9                              
031801     MOVE UPLP-IDDC          TO W-IDDC-D9                                 
031901                                                                          
032001     PERFORM IMS-GU-WDD901                                                
032101     IF SEGMENT-FINNS                                                     
032201        MOVE UPLP-KDAVROP    TO W-KDAVROP                                 
032301        PERFORM IMS-GHNP-WDD905                                           
032401        PERFORM UNTIL SEGMENT-SAKNAS                                      
032501           PERFORM IMS-DLET-WDD905                                        
032601           PERFORM IMS-GHNP-WDD905                                        
032701        END-PERFORM                                                       
032801     END-IF                                                               
032901     .                                                                    
033001                                                                          
033101                                                                          
033201 D-UPDATE-WDK722   SECTION.                                               
033301     MOVE 'D-UPDATE-WDK722 ' TO CURRENT-SECTION                           
033401                                                                          
033501     MOVE UPLP-IDARTNR       TO W-IDARTNR                                 
033601     MOVE UPLP-IDDC          TO W-IDDC                                    
033701                                                                          
033801     PERFORM IMS-GHU-WDK722                                               
033901     IF SEGMENT-FINNS                                                     
034001        MOVE UPLP-KDLEVPLF  TO XLAG-KDLEVPLF                              
034101        MOVE UPLP-KDLPSP    TO XLAG-KDLPSP                                
035001        MOVE UPLP-TILPSP    TO XLAG-TILPSP                                
036001        MOVE UPLP-TIOMSPEC  TO XLAG-TIOMSPEC                              
037001        PERFORM IMS-REPL-WDK722                                           
038001     END-IF                                                               
039001     .                                                                    
040001                                                                          
041001                                                                          
042001 E-NYUPPL-OMSPEC   SECTION.                                               
043001     MOVE 'E-NYUPPL-OMSPEC ' TO CURRENT-SECTION                           
044001                                                                          
045001     MOVE UPLP-IDARTNR       TO W-IDARTNR-D9                              
046001     MOVE UPLP-IDDC          TO W-IDDC-D9                                 
046101                                                                          
046201     PERFORM IMS-GU-WDD901                                                
046301     IF SEGMENT-SAKNAS                                                    
046401        MOVE UPLP-IDARTNR   TO D901-IDARTNR                               
046501        MOVE UPLP-IDDC      TO D901-IDDC                                  
046601        PERFORM IMS-ISRT-WDD901                                           
046701     END-IF                                                               
046801                                                                          
046901     MOVE UPLP-IDLEVNR       TO W-IDLEVNR                                 
047001                                                                          
048001     PERFORM IMS-GU-WDD902                                                
048101     IF SEGMENT-SAKNAS                                                    
048201        MOVE SPACE          TO D902-WDD902                                
048301        MOVE UPLP-IDLEVNR   TO D902-IDLEVNR                               
048401        MOVE ZERO           TO D902-KVBR                                  
048501        MOVE ZERO           TO D902-TILEVPL                               
048601        PERFORM IMS-ISRT-WDD902                                           
048701     END-IF                                                               
048801                                                                          
048901     MOVE UPLP-DASPECST        TO D904-DASPECST                           
049001     MOVE UPLP-KDLPORS-TAB (1) TO D904-KDLPORS-TAB (1)                    
049101     MOVE UPLP-KDLPORS-TAB (2) TO D904-KDLPORS-TAB (2)                    
049201     MOVE UPLP-KDLPORS-TAB (3) TO D904-KDLPORS-TAB (3)                    
049301     MOVE UPLP-KVBEST-PL       TO D904-KVBEST-PL                          
049401     MOVE UPLP-KDPLKOEP        TO D904-KDPLKOEP                           
049501                                                                          
049601     PERFORM IMS-ISRT-WDD904                                              
049701     .                                                                    
049801                                                                          
049901                                                                          
050001 F-UPPDAT-OMSPEC   SECTION.                                               
050101     MOVE 'F-UPPDAT-OMSPEC ' TO CURRENT-SECTION                           
050201                                                                          
050301     MOVE UPLP-IDARTNR       TO W-IDARTNR-D9                              
050401     MOVE UPLP-IDDC          TO W-IDDC-D9                                 
050501     MOVE UPLP-IDLEVNR       TO W-IDLEVNR                                 
050601                                                                          
050701     PERFORM IMS-GHU-WDD904                                               
050801                                                                          
050901     IF SEGMENT-FINNS                                                     
051001        MOVE UPLP-KDLPORS-TAB (1) TO D904-KDLPORS-TAB (1)                 
052001        MOVE UPLP-KDLPORS-TAB (2) TO D904-KDLPORS-TAB (2)                 
053001        MOVE UPLP-KDLPORS-TAB (3) TO D904-KDLPORS-TAB (3)                 
053101        MOVE UPLP-KVBEST-PL       TO D904-KVBEST-PL                       
053201        MOVE UPLP-KDPLKOEP        TO D904-KDPLKOEP                        
053301                                                                          
053401        PERFORM IMS-REPL-WDD904                                           
053501     ELSE                                                                 
053601*--- EFTERSOM W2241400 SKAPAR TRANS SOM RENSAR FÖRSLAGET                  
053701*--- OCH SEDAN SKAPAR NYA POSTER KAN BEFINTLIGT SEGMENT                   
053801*--- HA TAGITS BORT I DET HÄR PROGRAMMET FÅR VI GÖRA ETT NYUPPLÄGG        
053901        MOVE UPLP-DASPECST        TO D904-DASPECST                        
054001        MOVE UPLP-KDLPORS-TAB (1) TO D904-KDLPORS-TAB (1)                 
054101        MOVE UPLP-KDLPORS-TAB (2) TO D904-KDLPORS-TAB (2)                 
054201        MOVE UPLP-KDLPORS-TAB (3) TO D904-KDLPORS-TAB (3)                 
054301        MOVE UPLP-KVBEST-PL       TO D904-KVBEST-PL                       
054401        MOVE UPLP-KDPLKOEP        TO D904-KDPLKOEP                        
054501                                                                          
054601        PERFORM IMS-ISRT-WDD904                                           
054701     END-IF                                                               
054801                                                                          
054901     .                                                                    
055001                                                                          
055101                                                                          
055201 G-NYUPPL-LEV   SECTION.                                                  
055301     MOVE 'G-NYUPPL-LEV    ' TO CURRENT-SECTION                           
055401                                                                          
055501     MOVE UPLP-IDARTNR       TO W-IDARTNR-D9                              
055601     MOVE UPLP-IDDC          TO W-IDDC-D9                                 
055701     MOVE SPACE              TO D902-WDD902                               
055801     MOVE UPLP-IDLEVNR       TO D902-IDLEVNR                              
055901     MOVE ZERO               TO D902-KVBR                                 
056001     MOVE ZERO               TO D902-TILEVPL                              
056101                                                                          
056201     PERFORM IMS-GU-WDD901                                                
056301     IF SEGMENT-SAKNAS                                                    
056401        MOVE UPLP-IDARTNR    TO D901-IDARTNR                              
056501        MOVE UPLP-IDDC       TO D901-IDDC                                 
056601        PERFORM IMS-ISRT-WDD901                                           
056701     END-IF                                                               
056801                                                                          
056901     MOVE UPLP-IDLEVNR       TO W-IDLEVNR                                 
057001     PERFORM IMS-GU-WDD902                                                
057101     IF SEGMENT-SAKNAS                                                    
057201        MOVE SPACE           TO D902-WDD902                               
057301        MOVE UPLP-IDLEVNR    TO D902-IDLEVNR                              
057401        MOVE ZERO            TO D902-KVBR                                 
057501        MOVE ZERO            TO D902-TILEVPL                              
057601        PERFORM IMS-ISRT-WDD902                                           
057701     END-IF                                                               
057801     .                                                                    
057901                                                                          
058001                                                                          
058101 H-UPD-AVROP    SECTION.                                                  
058201     MOVE 'H-UPD-AVROP     ' TO CURRENT-SECTION                           
058301                                                                          
058401     MOVE UPLP-IDARTNR       TO W-IDARTNR-D9                              
058501     MOVE UPLP-IDDC          TO W-IDDC-D9                                 
058601     MOVE UPLP-IDLEVNR       TO W-IDLEVNR                                 
058701     MOVE UPLP-KDAVROP       TO W-KDAVROP                                 
058801     MOVE UPLP-DAAVROP-AVS   TO W-DAAVROP-AVS                             
058901     MOVE UPLP-TILEVDAG      TO W-TILEVDAG                                
059001                                                                          
059101     PERFORM IMS-GHU-WDD905                                               
059201     IF SEGMENT-FINNS                                                     
059301        ADD UPLP-KVAVROP        TO D905-KVAVROP                           
059401        PERFORM IMS-REPL-WDD905                                           
059501     ELSE                                                                 
059601*--- EFTERSOM W2241400 SKAPAR TRANS SOM RENSAR FÖRSLAGET                  
059701*--- OCH SEDAN SKAPAR NYA POSTER KAN BEFINTLIGT SEGMENT                   
059801*--- HA TAGITS BORT I DET HÄR PROGRAMMET FÅR VI GÖRA ETT NYUPPLÄGG        
059901        MOVE UPLP-IDARTNR       TO W-IDARTNR-D9                           
060001        MOVE UPLP-IDDC          TO W-IDDC-D9                              
060101        MOVE UPLP-IDLEVNR       TO W-IDLEVNR                              
060201        MOVE UPLP-KDAVROP       TO D905-KDAVROP                           
060301        MOVE UPLP-DAAVROP-AVS   TO D905-DAAVROP-AVS                       
060401        MOVE UPLP-TILEVDAG      TO D905-TILEVDAG                          
060501        MOVE UPLP-TIAVRDAT-INL  TO D905-TIAVRDAT-INL                      
060601        MOVE UPLP-TIAVRDAT-DISP TO D905-TIAVRDAT-DISP                     
060701        MOVE UPLP-KVAVROP       TO D905-KVAVROP                           
060801                                                                          
060901        PERFORM IMS-ISRT-WDD905                                           
061001     END-IF                                                               
061101     .                                                                    
061201                                                                          
061301                                                                          
061401 I-NYUPPL-AVROP SECTION.                                                  
061501     MOVE 'I-NYUPPL-AVROP  ' TO CURRENT-SECTION                           
061601                                                                          
061701     MOVE UPLP-IDARTNR       TO W-IDARTNR-D9                              
061801     MOVE UPLP-IDDC          TO W-IDDC-D9                                 
061901     MOVE UPLP-IDLEVNR       TO W-IDLEVNR                                 
062001     MOVE UPLP-KDAVROP       TO W-KDAVROP                                 
062101     MOVE UPLP-DAAVROP-AVS   TO W-DAAVROP-AVS                             
062201     MOVE UPLP-TILEVDAG      TO W-TILEVDAG                                
062401                                                                          
062501     PERFORM IMS-GHU-WDD905                                               
062601     IF SEGMENT-FINNS                                                     
062701                                                                          
062801*--- VID HELGFLYTT AV AVROP KAN DET HÄNDA ATT AVROPET+TILEVDAG            
062901*--- REDAN ÄR UPPLAGT TIDIGARE.ADDERA FLYTTADE KVANTEN ISÅFALL.           
063001                                                                          
063101        ADD UPLP-KVAVROP        TO D905-KVAVROP                           
063201        PERFORM IMS-REPL-WDD905                                           
063301     ELSE                                                                 
063501       MOVE UPLP-DAAVROP-AVS   TO D905-DAAVROP-AVS                        
063601       MOVE UPLP-TILEVDAG      TO D905-TILEVDAG                           
063701       MOVE UPLP-KDAVROP       TO D905-KDAVROP                            
063801       MOVE UPLP-TIAVRDAT-INL  TO D905-TIAVRDAT-INL                       
063901       MOVE UPLP-TIAVRDAT-DISP TO D905-TIAVRDAT-DISP                      
064001       MOVE UPLP-KVAVROP       TO D905-KVAVROP                            
064101                                                                          
064201       PERFORM IMS-ISRT-WDD905                                            
064301     END-IF                                                               
064501     .                                                                    
064601     EJECT                                                                
064701                                                                          
064801 Z-FINIT SECTION.                                                         
064901     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
065001                                                                          
065101     CLOSE W22415                                                         
065201                                                                          
065301     MOVE 'S' TO POSTSUM-OPKOD                                            
065401     CALL POSTSUM USING POSTSUM-PARM                                      
065501                                                                          
065601     PERFORM IMS-GHU-RESTART                                              
065701     MOVE ZERO       TO 4580-KVPOST                                       
065801     ACCEPT 4580-TIUPPDAT FROM DATE                                       
065901     ACCEPT 4580-TIUPPTID FROM TIME                                       
066001     PERFORM IMS-REPL-RESTART                                             
066101     .                                                                    
066201                                                                          
066301                                                                          
066401 S01-LAES-W22415  SECTION.                                                
066501                                                                          
066601     READ W22415 INTO UPLP-AREA                                           
066701     AT END                                                               
066801        SET END-OF-W22415 TO TRUE                                         
066901                                                                          
067001     NOT AT END                                                           
067101        MOVE 'W224155'    TO POSTSUM-FDNAMN                               
067201        MOVE 'W22413D1'   TO POSTSUM-DDNAMN2                              
067301        MOVE UPLP-IDPTYP  TO POSTSUM-TRANSTYP                             
067401        CALL POSTSUM USING   POSTSUM-PARM                                 
067501                                                                          
067601        ADD 1 TO W-ANTAL-POSTER                                           
067701     END-READ                                                             
067801     .                                                                    
067901                                                                          
068001                                                                          
068101 X-TAG-CHECKPOINT   SECTION.                                              
068201                                                                          
068301     PERFORM IMS-GHU-RESTART                                              
068401     MOVE W-ANTAL-POSTER TO 4580-KVPOST                                   
068501     ACCEPT 4580-TIUPPDAT FROM DATE                                       
068601     ACCEPT 4580-TIUPPTID FROM TIME                                       
068701     PERFORM IMS-REPL-RESTART                                             
068801                                                                          
068901     PERFORM IMS-CHECKPOINT                                               
069001     MOVE ZERO TO CHKP-ANT                                                
069101     .                                                                    
069201                                                                          
069301                                                                          
069401* --- IMS SEKTIONER ---                                                   
069501                                                                          
069601                                                                          
069701 IMS-RESTART SECTION.                                                     
069801     MOVE 'IMS-RESTART     ' TO CURRENT-IMS-SECTION                       
069901                                                                          
070001     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
070101     MOVE '  ' TO GODK-STATUSKODER                                        
070201     CALL CBLTDLI USING XRST MSG-PCB                                      
070301                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
070400                        CHKP-AREA-LENGTH CHKP-AREA                        
070501     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
070601     PERFORM IMS-STATUSKONTROLL                                           
070701     .                                                                    
070801                                                                          
070901                                                                          
071001 IMS-CHECKPOINT SECTION.                                                  
071101     MOVE 'IMS-CHECKPOINT  ' TO CURRENT-IMS-SECTION                       
071201                                                                          
071301     MOVE SPACE  TO CHKP-MSG-IO-AREA                                      
071401     MOVE '  XD' TO GODK-STATUSKODER                                      
071501     CALL CBLTDLI USING CHKP MSG-PCB                                      
071601                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
071701                        CHKP-AREA-LENGTH CHKP-AREA                        
071801     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
071901     PERFORM IMS-STATUSKONTROLL                                           
072001                                                                          
072101     IF IMS-EJ-OK                                                         
072201       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
072301       DISPLAY FELTEXT                                                    
072401       CALL FELLOG                                                        
072501     END-IF                                                               
072601     .                                                                    
072701                                                                          
072801                                                                          
072901 IMS-GHU-WDK711 SECTION.                                                  
073001     MOVE 'IMS-GHU-WDK711  ' TO CURRENT-IMS-SECTION                       
073101                                                                          
073201     MOVE SPACE               TO ALL-SSA                                  
073301     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
073401          DELIMITED BY SIZE INTO SSA1                                     
073501     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
073601          DELIMITED BY SIZE INTO SSA2                                     
073701     MOVE '  '                TO GODK-STATUSKODER                         
073801     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
073901     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
074001     PERFORM IMS-STATUSKONTROLL                                           
074101     .                                                                    
074201                                                                          
074301                                                                          
074401 IMS-REPL-WDK711 SECTION.                                                 
074501     MOVE 'IMS-REPL-WDK711 ' TO CURRENT-IMS-SECTION                       
074601                                                                          
074701     MOVE SPACE               TO ALL-SSA                                  
074801     MOVE '  '                TO GODK-STATUSKODER                         
074901     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
075001     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
075101     PERFORM IMS-STATUSKONTROLL                                           
075201     ADD +2 TO CHKP-ANT                                                   
075301     .                                                                    
075401                                                                          
075501                                                                          
075601 IMS-GHU-WDK722 SECTION.                                                  
075701     MOVE 'IMS-GHU-WDK722  ' TO CURRENT-IMS-SECTION                       
075801                                                                          
075901     MOVE SPACE               TO ALL-SSA                                  
076001     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
076101          DELIMITED BY SIZE INTO SSA1                                     
076201     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
076301          DELIMITED BY SIZE INTO SSA2                                     
076401     MOVE 'WDK722 '           TO SSA3                                     
076501     MOVE '  GE'              TO GODK-STATUSKODER                         
076601     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3         
076701     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
076801     PERFORM IMS-STATUSKONTROLL                                           
076901     .                                                                    
077001                                                                          
077101                                                                          
077201 IMS-REPL-WDK722 SECTION.                                                 
077301     MOVE 'IMS-REPL-WDK722 ' TO CURRENT-IMS-SECTION                       
077401                                                                          
077501     MOVE SPACE               TO ALL-SSA                                  
077601     MOVE '  '                TO GODK-STATUSKODER                         
077701     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK722                       
077801     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
077901     PERFORM IMS-STATUSKONTROLL                                           
078001     ADD +1 TO CHKP-ANT                                                   
078101     .                                                                    
078201                                                                          
078301                                                                          
078401 IMS-GU-WDD901 SECTION.                                                   
078501     MOVE 'IMS-GU-WDD901   ' TO CURRENT-IMS-SECTION                       
078601                                                                          
078701     MOVE SPACE               TO ALL-SSA                                  
078801     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
078901          DELIMITED BY SIZE INTO SSA1                                     
079001     MOVE '  GE'              TO GODK-STATUSKODER                         
079101     CALL CBLTDLI USING GU   WDD9-PCB DLI-IO-WDD901 SSA1                  
079201     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
079301     PERFORM IMS-STATUSKONTROLL                                           
079401     .                                                                    
079501                                                                          
079601                                                                          
079701 IMS-GHU-WDD901 SECTION.                                                  
079801     MOVE 'IMS-GHU-WDD901  ' TO CURRENT-IMS-SECTION                       
079901                                                                          
080001     MOVE SPACE               TO ALL-SSA                                  
080101     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
080201          DELIMITED BY SIZE INTO SSA1                                     
080301     MOVE '  '                TO GODK-STATUSKODER                         
080401     CALL CBLTDLI USING GHU  WDD9-PCB DLI-IO-WDD901 SSA1                  
080501     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
080601     PERFORM IMS-STATUSKONTROLL                                           
080701     .                                                                    
080801                                                                          
080901                                                                          
081001 IMS-ISRT-WDD901 SECTION.                                                 
081101     MOVE 'IMS-ISRT-WDD901 ' TO CURRENT-IMS-SECTION                       
081201                                                                          
081301     MOVE SPACE               TO ALL-SSA                                  
081401     MOVE 'WDD901 '           TO SSA1                                     
081501     MOVE '  '                TO GODK-STATUSKODER                         
081601     CALL CBLTDLI USING ISRT WDD9-PCB DLI-IO-WDD901 SSA1                  
081701     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
081801     PERFORM IMS-STATUSKONTROLL                                           
081901     ADD +1 TO CHKP-ANT                                                   
082001     .                                                                    
082101                                                                          
082201                                                                          
082301 IMS-DLET-WDD901 SECTION.                                                 
082401     MOVE 'IMS-DLET-WDD901 ' TO CURRENT-IMS-SECTION                       
082501                                                                          
082601     MOVE SPACE               TO ALL-SSA                                  
082701     MOVE '  '                TO GODK-STATUSKODER                         
082801     CALL CBLTDLI USING DLET WDD9-PCB DLI-IO-WDD901                       
082901     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
083001     PERFORM IMS-STATUSKONTROLL                                           
083101     ADD +1 TO CHKP-ANT                                                   
083201     .                                                                    
083301                                                                          
083401                                                                          
083501 IMS-GU-WDD902 SECTION.                                                   
083601     MOVE 'IMS-GU-WDD902   ' TO CURRENT-IMS-SECTION                       
083701                                                                          
083801     MOVE SPACE               TO ALL-SSA                                  
083901     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
084001          DELIMITED BY SIZE INTO SSA1                                     
084101     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
084201          DELIMITED BY SIZE INTO SSA2                                     
084301     MOVE '  GE'              TO GODK-STATUSKODER                         
084401     CALL CBLTDLI USING GU   WDD9-PCB DLI-IO-WDD902 SSA1 SSA2             
084501     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
084601     PERFORM IMS-STATUSKONTROLL                                           
084701     .                                                                    
084801                                                                          
084901                                                                          
085001 IMS-ISRT-WDD902 SECTION.                                                 
085101     MOVE 'IMS-ISRT-WDD902 ' TO CURRENT-IMS-SECTION                       
085201                                                                          
085301     MOVE SPACE               TO ALL-SSA                                  
085401     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
085501          DELIMITED BY SIZE INTO SSA1                                     
085601     MOVE 'WDD902 '           TO SSA2                                     
085701     MOVE '  '                TO GODK-STATUSKODER                         
085801     CALL CBLTDLI USING ISRT WDD9-PCB DLI-IO-WDD902 SSA1 SSA2             
085901     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
086001     PERFORM IMS-STATUSKONTROLL                                           
086101     ADD +1 TO CHKP-ANT                                                   
086201     .                                                                    
086301                                                                          
086401                                                                          
086501 IMS-DLET-WDD902 SECTION.                                                 
086601     MOVE 'IMS-DLET-WDD902 ' TO CURRENT-IMS-SECTION                       
086701                                                                          
086801     MOVE SPACE               TO ALL-SSA                                  
086901     MOVE '  '                TO GODK-STATUSKODER                         
087001     CALL CBLTDLI USING DLET WDD9-PCB DLI-IO-WDD902                       
087101     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
087201     PERFORM IMS-STATUSKONTROLL                                           
087301     ADD +1 TO CHKP-ANT                                                   
087401     .                                                                    
087501                                                                          
087601                                                                          
093401 IMS-GHU-WDD904 SECTION.                                                  
093501     MOVE 'IMS-GHU-WDD904  ' TO CURRENT-IMS-SECTION                       
093601                                                                          
093701     MOVE SPACE               TO ALL-SSA                                  
093801     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
093901          DELIMITED BY SIZE INTO SSA1                                     
094001     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
094101          DELIMITED BY SIZE INTO SSA2                                     
094201     MOVE   'WDD904 '         TO SSA3                                     
094301     MOVE '  GE'              TO GODK-STATUSKODER                         
094401     CALL CBLTDLI USING GHU  WDD9-PCB DLI-IO-WDD904 SSA1 SSA2 SSA3        
094501     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
094601     PERFORM IMS-STATUSKONTROLL                                           
094701     .                                                                    
094801                                                                          
094901                                                                          
095001 IMS-GHNP-WDD904 SECTION.                                                 
095101     MOVE 'IMS-GHNP-WDD904 ' TO CURRENT-IMS-SECTION                       
095201                                                                          
095301     MOVE SPACE               TO ALL-SSA                                  
095401     MOVE 'WDD902 '           TO SSA1                                     
095501     MOVE 'WDD904 '           TO SSA2                                     
095600     MOVE '  GE'              TO GODK-STATUSKODER                         
095700     CALL CBLTDLI USING GHNP WDD9-PCB DLI-IO-WDD904 SSA1 SSA2             
095800     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
095900     PERFORM IMS-STATUSKONTROLL                                           
096000     .                                                                    
096100                                                                          
096200                                                                          
096301 IMS-ISRT-WDD904 SECTION.                                                 
096401     MOVE 'IMS-ISRT-WDD904 ' TO CURRENT-IMS-SECTION                       
096501                                                                          
096601     MOVE SPACE               TO ALL-SSA                                  
096701     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
096801          DELIMITED BY SIZE INTO SSA1                                     
096901     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
097001          DELIMITED BY SIZE INTO SSA2                                     
097101     MOVE 'WDD904 '           TO SSA3                                     
097201     MOVE '  '                TO GODK-STATUSKODER                         
097301     CALL CBLTDLI USING ISRT WDD9-PCB DLI-IO-WDD904 SSA1 SSA2 SSA3        
097401     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
097501     PERFORM IMS-STATUSKONTROLL                                           
097601     ADD +1 TO CHKP-ANT                                                   
097701     .                                                                    
097801                                                                          
097901                                                                          
098001 IMS-REPL-WDD904 SECTION.                                                 
098101     MOVE 'IMS-REPL-WDD904 ' TO CURRENT-IMS-SECTION                       
098201                                                                          
098301     MOVE SPACE               TO ALL-SSA                                  
098401     MOVE '  '                TO GODK-STATUSKODER                         
098501     CALL CBLTDLI USING REPL WDD9-PCB DLI-IO-WDD904                       
098601     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
098701     PERFORM IMS-STATUSKONTROLL                                           
098801     ADD +1 TO CHKP-ANT                                                   
098901     .                                                                    
099001                                                                          
099101                                                                          
099201 IMS-DLET-WDD904 SECTION.                                                 
099301     MOVE 'IMS-DLET-WDD904 ' TO CURRENT-IMS-SECTION                       
099401                                                                          
099501     MOVE SPACE               TO ALL-SSA                                  
099601     MOVE '  '                TO GODK-STATUSKODER                         
099701     CALL CBLTDLI USING DLET WDD9-PCB DLI-IO-WDD904                       
099801     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
099901     PERFORM IMS-STATUSKONTROLL                                           
100001     ADD +1 TO CHKP-ANT                                                   
100101     .                                                                    
100201                                                                          
100301                                                                          
100401 IMS-GHU-WDD905 SECTION.                                                  
100501     MOVE 'IMS-GHU-WDD905  ' TO CURRENT-IMS-SECTION                       
100601                                                                          
100701     MOVE SPACE               TO ALL-SSA                                  
100801     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
100901          DELIMITED BY SIZE INTO SSA1                                     
101001     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
101101          DELIMITED BY SIZE INTO SSA2                                     
101201     STRING 'WDD905  (WDD905KY =' W-WDD905KY-X                            
101301                    '&KDAVROP  =' W-KDAVROP-X ')'                         
101401          DELIMITED BY SIZE INTO SSA3                                     
101501     MOVE '  GE'              TO GODK-STATUSKODER                         
101601     CALL CBLTDLI USING GHU  WDD9-PCB DLI-IO-WDD905 SSA1 SSA2 SSA3        
101701     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
101801     PERFORM IMS-STATUSKONTROLL                                           
101901     .                                                                    
102001                                                                          
102101                                                                          
102201 IMS-GHNP-WDD905   SECTION.                                               
102301     MOVE 'IMS-GHNP-WDD905 ' TO CURRENT-IMS-SECTION                       
102401                                                                          
102501     MOVE SPACE               TO ALL-SSA                                  
102601     MOVE 'WDD902 '           TO SSA1                                     
102701     STRING 'WDD905  (KDAVROP  =' W-KDAVROP-X ')'                         
102801          DELIMITED BY SIZE INTO SSA2                                     
102901     MOVE '  GE'              TO GODK-STATUSKODER                         
103001     CALL CBLTDLI USING GHNP WDD9-PCB DLI-IO-WDD905 SSA1 SSA2             
103101     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
103201     PERFORM IMS-STATUSKONTROLL                                           
103301     .                                                                    
103401                                                                          
103501                                                                          
103601 IMS-DLET-WDD905 SECTION.                                                 
103701     MOVE 'IMS-DLET-WDD905 ' TO CURRENT-IMS-SECTION                       
103801                                                                          
103901     MOVE SPACE               TO ALL-SSA                                  
104001     MOVE '  '                TO GODK-STATUSKODER                         
104101     CALL CBLTDLI USING DLET WDD9-PCB DLI-IO-WDD905                       
104201     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
104301     PERFORM IMS-STATUSKONTROLL                                           
104401     ADD +1 TO CHKP-ANT                                                   
104501     .                                                                    
104601                                                                          
104701                                                                          
104801 IMS-REPL-WDD905 SECTION.                                                 
104901     MOVE 'IMS-REPL-WDD905 ' TO CURRENT-IMS-SECTION                       
105001                                                                          
105101     MOVE SPACE               TO ALL-SSA                                  
105201     MOVE '  '                TO GODK-STATUSKODER                         
105301     CALL CBLTDLI USING REPL WDD9-PCB DLI-IO-WDD905                       
105401     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
105501     PERFORM IMS-STATUSKONTROLL                                           
105601     ADD +1 TO CHKP-ANT                                                   
105701     .                                                                    
105801                                                                          
105901                                                                          
106001 IMS-ISRT-WDD905 SECTION.                                                 
106101     MOVE 'IMS-ISRT-WDD905 ' TO CURRENT-IMS-SECTION                       
106201                                                                          
106301     MOVE SPACE               TO ALL-SSA                                  
106401     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
106501          DELIMITED BY SIZE INTO SSA1                                     
106601     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
106701          DELIMITED BY SIZE INTO SSA2                                     
106801     MOVE 'WDD905 '           TO SSA3                                     
106901     MOVE '  '                TO GODK-STATUSKODER                         
107001     CALL CBLTDLI USING ISRT WDD9-PCB DLI-IO-WDD905 SSA1 SSA2 SSA3        
107101     MOVE WDD9-STATUS-CODE    TO STATUS-WS                                
107201     PERFORM IMS-STATUSKONTROLL                                           
107301     ADD +1 TO CHKP-ANT                                                   
107401     .                                                                    
107501                                                                          
107601                                                                          
107701 IMS-GHU-WDD601 SECTION.                                                  
107801     MOVE 'IMS-GHU-WDD601  ' TO CURRENT-IMS-SECTION                       
107901                                                                          
108001     MOVE SPACE               TO ALL-SSA                                  
108101     STRING 'WDD601  (WDD601KY =' W-WDD601KY-X ')'                        
108201          DELIMITED BY SIZE INTO SSA1                                     
108301     MOVE '  '                TO GODK-STATUSKODER                         
108401     CALL CBLTDLI USING GHU  WDD6-PCB DLI-IO-WDD601 SSA1                  
108501     MOVE WDD6-STATUS-CODE    TO STATUS-WS                                
108601     PERFORM IMS-STATUSKONTROLL                                           
108701     .                                                                    
108801                                                                          
108901                                                                          
109001 IMS-DLET-WDD601 SECTION.                                                 
109101     MOVE 'IMS-DLET-WDD601 ' TO CURRENT-IMS-SECTION                       
109201                                                                          
109301     MOVE SPACE               TO ALL-SSA                                  
109401     MOVE '  '                TO GODK-STATUSKODER                         
109501     CALL CBLTDLI USING DLET WDD6-PCB DLI-IO-WDD601                       
109601     MOVE WDD6-STATUS-CODE    TO STATUS-WS                                
109701     PERFORM IMS-STATUSKONTROLL                                           
109801     ADD +1 TO CHKP-ANT                                                   
109901     .                                                                    
110001                                                                          
110101                                                                          
110201 IMS-GHU-RESTART  SECTION.                                                
110301     MOVE 'IMS-GHU-RESTART '  TO CURRENT-IMS-SECTION                      
110401                                                                          
110501     MOVE SPACE          TO ALL-SSA                                       
110601     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4579-X ')'                    
110701          DELIMITED BY SIZE INTO SSA1                                     
110801     MOVE 'WDR470   '    TO SSA2                                          
110900     MOVE '    '         TO GODK-STATUSKODER                              
111001     CALL CBLTDLI USING GHU 4579-PCB DLI-IO-WDGX4580 SSA1 SSA2            
111101     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
111201     PERFORM IMS-STATUSKONTROLL                                           
111301     .                                                                    
111401                                                                          
111501                                                                          
111601 IMS-REPL-RESTART SECTION.                                                
111701     MOVE 'IMS-REPL-RESTART'  TO CURRENT-IMS-SECTION                      
111801                                                                          
111901     MOVE '  '             TO GODK-STATUSKODER                            
112001     CALL CBLTDLI USING REPL 4579-PCB DLI-IO-WDGX4580                     
112101     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
112201     PERFORM IMS-STATUSKONTROLL                                           
112301     .                                                                    
112401                                                                          
112501                                                                          
112601 IMS-STATUSKONTROLL SECTION.                                              
112701                                                                          
112801     SET STATUS-IX TO 1                                                   
112901     SEARCH GODK-STATUS                                                   
113001       AT END                                                             
113101         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
113201           DELIMITED BY SIZE INTO FELTEXT                                 
113301         DISPLAY FELTEXT                                                  
113401         CALL FELLOG                                                      
113501       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
113601         CONTINUE                                                         
113701     END-SEARCH                                                           
114001     .                                                                    
