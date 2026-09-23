000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2714500.                                                
000400*AUTHOR.         FRONTEC, GÖTEBORG.                                       
000500*DATE-WRITTEN.   95/01/03.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PERIODFLYTT - FLYTTA INNEVARANDE PERIODS FÖRBRUKNING TILL        
001100*        FÖREGÅENDE. VID ÅRETS SISTA KÖRNING SÅ FLYTTAS ÄVEN HELA         
001200*        ÅRSFÖRBRUKNINGEN.                                                
001300*                                                                         
001400*        PROGRAMMET UPPDATERAR WDL7 + WDL4                                
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP3                                                                
003000 FILE SECTION.                                                            
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300     SKIP2                                                                
003400                                                                          
003500*    -- CHECKED BY WY2000                                                 
003600 77  IDPGM                       PIC X(8)    VALUE 'W2714500'.            
003700 01  CHKP-VAR.                                                            
003800 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
003900 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
004000 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
004100 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
004200 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
004300 03  CHKP-MAX                    PIC S9(3)   VALUE +800.                  
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004704 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
004804 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
004904     SKIP2                                                                
005005 01  L411-VALUE                  PIC 99.                                  
005105     88  L411-EMPTY              VALUE 00.                                
005204 01  FELTEXT.                                                             
005304     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005404     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005504     EJECT                                                                
005604 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005704 01  FILLER REDEFINES DAGENS-DATUM.                                       
005804     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005904     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006004     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006104     EJECT                                                                
006204 01  ARBETSFAELT.                                                         
006304                                                                          
006404     03  W-INNEV-PER             PIC 9(4)    VALUE ZERO.                  
006504     03  FILLER REDEFINES W-INNEV-PER.                                    
006604         05 W-INNEV-AA           PIC 9(2).                                
006704         05 W-INNEV-PP           PIC 9(2).                                
006804     03  W-NAESTA-PER            PIC 9(4)    VALUE ZERO.                  
006904     03  FILLER REDEFINES W-NAESTA-PER.                                   
007004         05 W-NAESTA-AA          PIC 9(2).                                
007104         05 W-NAESTA-PP          PIC 9(2).                                
007204     03  WS-PERIOD-AARP          PIC 9(4)    VALUE ZERO.                  
007304     03  FILLER REDEFINES WS-PERIOD-AARP.                                 
007404         05 WS-PERIOD-AA         PIC 9(2).                                
007504         05 WS-PERIOD-RP         PIC 9(2).                                
007604     03  W-FORSTA-VV-INNEV       PIC 9(2)    VALUE ZERO.                  
007704     03  W-SISTA-VV-INNEV        PIC 9(2)    VALUE ZERO.                  
007804     03  W-FORSTA-VV-NAESTA      PIC 9(2)    VALUE ZERO.                  
007904     03  W-SISTA-VV-NAESTA       PIC 9(2)    VALUE ZERO.                  
008004     03  W-KVOT-FOREG            PIC S9(7)   VALUE ZERO COMP-3.           
008104     03  W-KVOT-CDC-FOREG        PIC S9(7)   VALUE ZERO COMP-3.           
008204     03  W-KVOT-REF-FOREG        PIC S9(7)   VALUE ZERO COMP-3.           
008304     03  WS-KVOI                 PIC S9(7)   VALUE ZERO COMP-3.           
008404     03  WS-KVOI-REFILL          PIC S9(7)   VALUE ZERO COMP-3.           
008504     03  WS-KVVIPER              PIC 9(1)    VALUE ZERO.                  
008604     03  W-TIAAVV                PIC 9(4)    VALUE ZERO.                  
008704     03  FILLER REDEFINES W-TIAAVV.                                       
008804         05 W-TIAA               PIC 9(2).                                
008904         05 W-TIVV               PIC 9(2).                                
009004     03  W-VV-IX                 PIC 9(2)    VALUE ZERO.                  
009104     03  W-INDX                  PIC 9(2)    VALUE ZERO.                  
009204     03  PER-IX                  PIC 9(2)    VALUE ZERO.                  
009304     03  RULL-IX                 PIC 9(2)    VALUE ZERO.                  
009404     03  RULL-IX-MAX             PIC 9(2)    VALUE ZERO.                  
009504     EJECT                                                                
009604 01  DYNAMISKA-SUBPROGRAM.                                                
009704*                                                                         
009804     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009904     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010004     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
010104     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
010204     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010304     EJECT                                                                
010404*    --- PARAMETRAR TILL DATKORT                                          
010504*                                                                         
010604 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W27145'.              
010704     SKIP2                                                                
010804 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
010904     SKIP2                                                                
011004*01  -COPY WDATKORT                                                       
011104     EJECT                                                                
011204*01  -COPY WDATAREA                                                       
011304*                                                                         
011404     EJECT                                                                
011504 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011604     SKIP3                                                                
011704 01  NYCKLAR-TILL-DLI.                                                    
011804     03  W-IDARTNR-X.                                                     
011904         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
012004     03  W-IDDC-X.                                                        
012104         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
012204     SKIP2                                                                
012304*    --- STATUS-KOD FRÅN IMS                                              
012404 01  STATUS-WS                   PIC XX.                                  
012504     88  SEGMENT-FINNS                       VALUE '  '.                  
012604     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012704     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012804     88  SEGMENT-SLUT                        VALUE 'GB'.                  
012904     88  IMS-EJ-OK                           VALUE 'XD'.                  
013004     SKIP2                                                                
013104 01  GODK-STATUSKODER.                                                    
013204     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013304     SKIP3                                                                
013404 01  SSA1                        PIC X(64).                               
013504 01  SSA2                        PIC X(64).                               
013604     EJECT                                                                
013704*    --- IMS FUNKTIONSKODER                                               
013804*01  -COPY W0003                                                          
013904     EJECT                                                                
014004*    ---  DLI INPUT-OUTPUT AREA                                           
014104 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-L701'.         
014200 01  DLI-IO-L701.                                                         
014400*    03  -COPY WDL701                                                     
014800     EJECT                                                                
014900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-L711'.         
015000 01  DLI-IO-L711.                                                         
015100*    03  -COPY WDL711                                                     
015200     EJECT                                                                
015307 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-L401'.         
015407 01  DLI-IO-L401.                                                         
015507*    03  -COPY WDL401                                                     
015600     EJECT                                                                
015707 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-L411'.         
015807 01  DLI-IO-L411.                                                         
015907*    03  -COPY WDL411                                                     
016007     EJECT                                                                
016107 LINKAGE SECTION.                                                         
016207                                                                          
016307*01  -COPY W0009   -PRE MSG-                                              
016407     EJECT                                                                
016507*01  -COPY W0008  -PRE WDL7-                                              
016607     05  FILLER                  PIC X.                                   
016707     EJECT                                                                
016807*01  -COPY W0008  -PRE WDL4-                                              
016907     05  FILLER                  PIC X.                                   
017007     EJECT                                                                
017107 PROCEDURE DIVISION  USING MSG-PCB WDL7-PCB WDL4-PCB.                     
017207     ENTRY 'DLITCBL' USING MSG-PCB WDL7-PCB WDL4-PCB.                     
017307                                                                          
017407     SKIP2                                                                
017507     PERFORM A-INIT                                                       
017607                                                                          
017707     PERFORM IMS-GN-WDL701                                                
017807                                                                          
017907     PERFORM UNTIL SEGMENT-SLUT                                           
018007                                                                          
018107       IF CHKP-ANT > CHKP-MAX                                             
018207         PERFORM X-TAG-CHECKPOINT                                         
018307       END-IF                                                             
018407       PERFORM IMS-GHNP-WDL711                                            
018507                                                                          
018607       PERFORM UNTIL SEGMENT-SAKNAS                                       
018707                                                                          
018807         PERFORM B-UPPDATERA-IDDC                                         
018907                                                                          
019007         PERFORM IMS-GHNP-WDL711                                          
019107                                                                          
019207       END-PERFORM                                                        
019307                                                                          
019407       PERFORM IMS-GN-WDL701                                              
019507                                                                          
019607     END-PERFORM                                                          
019707     MOVE ZERO TO RETURN-CODE                                             
019807     GOBACK                                                               
019907     .                                                                    
020007     EJECT                                                                
020107 A-INIT SECTION.                                                          
020207     SKIP2                                                                
020307                                                                          
020407     PERFORM IMS-RESTART                                                  
020507                                                                          
020607                                                                          
020707     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
020708     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
020907     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
021007     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
021107                                                                          
021207     PERFORM AA-INNEV-PERIOD                                              
021307     PERFORM AB-NAESTA-PERIOD                                             
021407     .                                                                    
021507     EJECT                                                                
021607 AA-INNEV-PERIOD SECTION.                                                 
021707                                                                          
021807     MOVE 'AAMMDD' TO DAT-KDDATFORM                                       
021907                                                                          
022007     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
022107                                                                          
022207*--- INNEVARANDE PERIOD                                                   
022307                                                                          
022407     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
022507                         DAT-O-TIDATUM DAT-KDSVAR                         
022607                                                                          
022707     IF DAT-KDSVAR-OK                                                     
022807*                                                                         
022907*--- FÖRSTA VECKAN I INNEVARANDE PERIOD                                   
023007*                                                                         
023107       MOVE DAT-TIAARP TO W-INNEV-PER                                     
023207                          DAT-I-TIDATUM                                   
023307       IF W-INNEV-PP = 1                                                  
023407         MOVE 1 TO W-FORSTA-VV-INNEV                                      
023507       ELSE                                                               
023607         MOVE 'AARP  '   TO DAT-KDDATFORM                                 
023707         CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                  
023807                             DAT-O-TIDATUM DAT-KDSVAR                     
023907         IF DAT-KDSVAR-OK                                                 
024007           MOVE DAT-TIVV TO W-FORSTA-VV-INNEV                             
024107         ELSE                                                             
024207           MOVE 'FEL I DATKONV1' TO FELTEXT-STR                           
024307           DISPLAY FELTEXT                                                
024407           CALL ABEND USING RKOD-ABEND-UTAN-DUMP                          
024507         END-IF                                                           
024607         PERFORM AAA-KOLLA-OM-52-EL-53-V                                  
024707       END-IF                                                             
024807     ELSE                                                                 
024907       MOVE 'FEL I DATKONV2' TO FELTEXT-STR                               
025007       DISPLAY FELTEXT                                                    
025107       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
025207     END-IF                                                               
025307     .                                                                    
025407     EJECT                                                                
025507 AAA-KOLLA-OM-52-EL-53-V SECTION.                                         
025607***                                                                       
025707* TAG REDA PÅ OM DET ÄR 52 ELLER 53 VECKOR PÅ ÅRET                        
025807***                                                                       
025907     MOVE W-INNEV-AA TO W-TIAA                                            
026007     MOVE 53         TO W-TIVV                                            
026107     MOVE W-TIAAVV   TO DAT-I-TIDATUM                                     
026207     MOVE 'AAVV  '   TO DAT-KDDATFORM                                     
026307     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
026407                         DAT-O-TIDATUM DAT-KDSVAR                         
026507     IF DAT-KDSVAR-OK                                                     
026607       MOVE 53 TO W-SISTA-VV-INNEV                                        
026707     ELSE                                                                 
026807       MOVE 52 TO W-SISTA-VV-INNEV                                        
026907     END-IF                                                               
027007     .                                                                    
027107     EJECT                                                                
027207 AB-NAESTA-PERIOD SECTION.                                                
027307***                                                                       
027407* HÄMTA NÄSTA PERIOD OCH FÖRSTA VECKAN I NÄSTA PERIOD                     
027507***                                                                       
027607     IF W-INNEV-PP = 12                                                   
027707*                                                                         
027807* --- PERIOD 1 BÖRJAR ALLTID MED V1                                       
027907*                                                                         
028007       MOVE +1 TO W-FORSTA-VV-NAESTA                                      
028107                  W-NAESTA-PP                                             
028207       COMPUTE W-NAESTA-AA = W-INNEV-AA + 1                               
028307     ELSE                                                                 
028407       MOVE W-INNEV-AA   TO W-NAESTA-AA                                   
028507       COMPUTE W-NAESTA-PP = W-INNEV-PP + 1                               
028607       MOVE W-NAESTA-PER TO DAT-I-TIDATUM                                 
028707       MOVE 'AARP'       TO DAT-KDDATFORM                                 
028807       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
028907                           DAT-O-TIDATUM DAT-KDSVAR                       
029007       IF DAT-KDSVAR-OK                                                   
029107         MOVE DAT-TIVV TO W-FORSTA-VV-NAESTA                              
029207       ELSE                                                               
029307         MOVE 'FEL I DATKONV3' TO FELTEXT-STR                             
029407         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
029507       END-IF                                                             
029607     END-IF                                                               
029707*                                                                         
029807*--- SISTA VECKAN I NÄSTA PERIOD                                          
029907*                                                                         
030007     IF W-NAESTA-PP = 12                                                  
030107*                                                                         
030207*--- TAG REDA PÅ OM DET ÄR 52 ELLER 53 VECKOR PÅ ÅRET                     
030307*                                                                         
030407       MOVE W-NAESTA-AA TO W-TIAA                                         
030507       MOVE 53          TO W-TIVV                                         
030607       MOVE W-TIAAVV    TO DAT-I-TIDATUM                                  
030707       MOVE 'AAVV  '    TO DAT-KDDATFORM                                  
030807       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
030907                           DAT-O-TIDATUM DAT-KDSVAR                       
031007       IF DAT-KDSVAR-OK                                                   
031107         MOVE 53 TO W-SISTA-VV-NAESTA                                     
031207       ELSE                                                               
031307         MOVE 52 TO W-SISTA-VV-NAESTA                                     
031407       END-IF                                                             
031507     ELSE                                                                 
031607       COMPUTE W-NAESTA-PP = W-NAESTA-PP + 1                              
031707       MOVE W-NAESTA-PER TO DAT-I-TIDATUM                                 
031807       MOVE 'AARP  '   TO DAT-KDDATFORM                                   
031907       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
032007                           DAT-O-TIDATUM DAT-KDSVAR                       
032107       IF DAT-KDSVAR-OK                                                   
032207         COMPUTE W-SISTA-VV-NAESTA = DAT-TIVV - 1                         
032307       ELSE                                                               
032407         MOVE 'FEL I DATKONV4' TO FELTEXT-STR                             
032507         DISPLAY FELTEXT                                                  
032607         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
032707       END-IF                                                             
032807     END-IF                                                               
032907     .                                                                    
033007     EJECT                                                                
033107 B-UPPDATERA-IDDC SECTION.                                                
033207                                                                          
033307     IF DC-TIVV(1) = W-FORSTA-VV-INNEV                                    
033407*                                                                         
033507*--- SEGMENTET ÄR INTE UPPDATERAT                                         
033607*                                                                         
033707       MOVE +1 TO W-INDX                                                  
033807       PERFORM UNTIL W-INDX > 5                                           
033907         IF DC-TIVV(W-INDX) > 0                                           
034007           MOVE DC-TIVV(W-INDX) TO W-VV-IX                                
034107           MOVE DC-KVOI-INNEV(W-INDX) TO                                  
034207                DC-KVOI-RULL(W-VV-IX)                                     
034208           ADD  DC-KVOI-PP-INNEV(W-INDX) TO                               
034209                DC-KVOI-RULL(W-VV-IX)                                     
034307           MOVE DC-KVOI-REF-INNEV(W-INDX) TO                              
034407                DC-KVOI-REF-RULL(W-VV-IX)                                 
034507           MOVE DC-KVOI-CDC-INNEV(W-INDX) TO                              
034607                DC-KVOI-CDC-RULL(W-VV-IX)                                 
034707           MOVE DC-KVOT-INNEV(W-INDX) TO                                  
034807                DC-KVOT-RULL(W-VV-IX)                                     
034808           ADD  DC-KVOT-PP-INNEV(W-INDX) TO                               
034809                DC-KVOT-RULL(W-VV-IX)                                     
034907           MOVE DC-KVOT-REF-INNEV(W-INDX) TO                              
035007                DC-KVOT-REF-RULL(W-VV-IX)                                 
035107           MOVE DC-KVOT-CDC-INNEV(W-INDX) TO                              
035207                DC-KVOT-CDC-RULL(W-VV-IX)                                 
035307         END-IF                                                           
035407         ADD +1 TO W-INDX                                                 
035507       END-PERFORM                                                        
035607                                                                          
035707                                                                          
035807       PERFORM BA-UPPD-NAESTA-PER-VV                                      
035907*                                                                         
036007*--- GÖRS ENDAST ÅRETS SISTA PERIOD                                       
036107*                                                                         
036207*      DISPLAY 'W-INNEV-PP = ' W-INNEV-PP                                 
036307       IF W-INNEV-PP = 12                                                 
036407         PERFORM BB-FLYTTA-ARS-SALDON                                     
036507       END-IF                                                             
036607                                                                          
036707       PERFORM IMS-REPL-WDL711                                            
036807       ADD +1 TO CHKP-ANT                                                 
036907                                                                          
037007     ELSE                                                                 
037107*                                                                         
037207*--- SEGMENTET ÄR REDAN UPPDATERAT                                        
037307*                                                                         
037407       CONTINUE                                                           
037507     END-IF                                                               
037607     .                                                                    
037707     EJECT                                                                
037807 BA-UPPD-NAESTA-PER-VV SECTION.                                           
037907                                                                          
038007***                                                                       
038107*  INITIERA FÄLT SAMT VECKONUMMER I NÄSTA PERIOD                          
038207***                                                                       
038307                                                                          
038407     MOVE +1 TO W-INDX                                                    
038507     MOVE W-FORSTA-VV-NAESTA TO W-VV-IX                                   
038607                                                                          
038707     PERFORM UNTIL W-INDX > 5                                             
038807                                                                          
038907       PERFORM UNTIL W-INDX > 5 OR W-VV-IX > W-SISTA-VV-NAESTA            
039007         MOVE W-VV-IX TO DC-TIVV(W-INDX)                                  
039107         MOVE ZERO  TO DC-KVOI-INNEV    (W-INDX)                          
039108                       DC-KVOI-PP-INNEV (W-INDX)                          
039207                       DC-KVOI-CDC-INNEV(W-INDX)                          
039307                       DC-KVOI-REF-INNEV(W-INDX)                          
039407                       DC-KVOT-INNEV    (W-INDX)                          
039408                       DC-KVOT-PP-INNEV (W-INDX)                          
039507                       DC-KVOT-CDC-INNEV(W-INDX)                          
039607                       DC-KVOT-REF-INNEV(W-INDX)                          
039707         ADD +1 TO W-VV-IX W-INDX                                         
039807       END-PERFORM                                                        
039907                                                                          
040007       IF W-INDX > 5                                                      
040107         CONTINUE                                                         
040207       ELSE                                                               
040307*                                                                         
040407*---  NOLLA RESTERANDE INDX OM FÄRRE ÄN 5V I PER                          
040507*                                                                         
040607         MOVE ZERO  TO DC-TIVV(W-INDX)                                    
040707                       DC-KVOI-INNEV(W-INDX)                              
040708                       DC-KVOI-PP-INNEV(W-INDX)                           
040807                       DC-KVOI-CDC-INNEV(W-INDX)                          
040907                       DC-KVOI-REF-INNEV(W-INDX)                          
041007                       DC-KVOT-INNEV(W-INDX)                              
041008                       DC-KVOT-PP-INNEV(W-INDX)                           
041107                       DC-KVOT-CDC-INNEV(W-INDX)                          
041207                       DC-KVOT-REF-INNEV(W-INDX)                          
041307         ADD +1 TO W-INDX                                                 
041407       END-IF                                                             
041507     END-PERFORM                                                          
041607     .                                                                    
041707     EJECT                                                                
041807 BB-FLYTTA-ARS-SALDON SECTION.                                            
041907                                                                          
042007     MOVE ART-IDARTNR TO W-IDARTNR                                        
042107     MOVE DC-IDDC     TO W-IDDC                                           
042207     PERFORM IMS-GHU-WDL411                                               
042307     IF SEGMENT-SAKNAS                                                    
042407****** IF PART/DC FROM WDL7 IS MISSING IN WDL4 THEN L411-AREA             
042507****** MUST BE ZEROED BCZ SOME ITEMS ARE USED IN COMPUTES BELOW           
042607       INITIALIZE OIHD-WDL411                                             
042707     END-IF                                                               
042807                                                                          
042907     MOVE ZERO TO L411-VALUE                                              
043007                                                                          
043107     PERFORM BBA-FLYTTA-KVOT-FOREG                                        
043207     PERFORM BBB-FLYTTA-KVOI-ARS-FORBRUKN                                 
043307                                                                          
043407     IF SEGMENT-FINNS                                                     
043507       PERFORM IMS-REPL-WDL411                                            
043607       ADD +1 TO CHKP-ANT                                                 
043707     ELSE                                                                 
043807       IF NOT L411-EMPTY                                                  
043907         MOVE ART-IDARTNR TO OIHA-IDARTNR                                 
044007         PERFORM IMS-ISRT-WDL401                                          
044107         MOVE DC-IDDC     TO OIHD-IDDC                                    
044207         PERFORM IMS-ISRT-WDL411                                          
044307       END-IF                                                             
044407     END-IF                                                               
044507     .                                                                    
044607     EJECT                                                                
044707                                                                          
044807                                                                          
044907 BBA-FLYTTA-KVOT-FOREG SECTION.                                           
045007***                                                                       
045107* FLYTTA FÖREGÅENDE ÅRS FÖRSÄLJNING TILL FÖRFÖRGÅENDE                     
045207***                                                                       
045307     MOVE OIHD-KVOT-FOREG(1) TO                                           
045407          OIHD-KVOT-FOREG(2)                                              
045507          L411-VALUE                                                      
045607     MOVE OIHD-KVOT-CDC-FOREG(1) TO                                       
045707          OIHD-KVOT-CDC-FOREG(2)                                          
045807          L411-VALUE                                                      
045907     MOVE OIHD-KVOT-REF-FOREG(1) TO                                       
046007          OIHD-KVOT-REF-FOREG(2)                                          
046107          L411-VALUE                                                      
046305*                                                                         
046405*--- KONTROLLERA OM V 53 SKA RENSAS, DVS INNEVARANDE ÅR HAR               
046505*    52 V OCH FÖREGÅENDE HADE EV 53.                                      
046605*                                                                         
046705     IF W-SISTA-VV-INNEV = 52                                             
046805       MOVE ZERO TO DC-KVOT-RULL(53)                                      
046905                    DC-KVOT-REF-RULL(53)                                  
047005                    DC-KVOT-CDC-RULL(53)                                  
047105     END-IF                                                               
047205*                                                                         
047305*--- SUMMERA ÅRETS FÖRSÄLJNING OCH LÄGG I FÖREGÅENDE ÅR                   
047405*                                                                         
047505     MOVE +1 TO W-INDX                                                    
047605     MOVE ZERO TO W-KVOT-FOREG W-KVOT-CDC-FOREG W-KVOT-REF-FOREG          
047705                                                                          
047805     PERFORM UNTIL W-INDX > 53                                            
047905       ADD DC-KVOT-RULL(W-INDX)     TO W-KVOT-FOREG                       
048005       ADD DC-KVOT-CDC-RULL(W-INDX) TO W-KVOT-CDC-FOREG                   
048105       ADD DC-KVOT-REF-RULL(W-INDX) TO W-KVOT-REF-FOREG                   
048205       ADD +1 TO W-INDX                                                   
048305     END-PERFORM                                                          
048405                                                                          
048505     MOVE W-KVOT-FOREG     TO OIHD-KVOT-FOREG(1)                          
048605                              L411-VALUE                                  
048905     MOVE W-KVOT-CDC-FOREG TO OIHD-KVOT-CDC-FOREG(1)                      
049005                              L411-VALUE                                  
049305     MOVE W-KVOT-REF-FOREG TO OIHD-KVOT-REF-FOREG(1)                      
049405                              L411-VALUE                                  
049705     .                                                                    
049805     EJECT                                                                
049905                                                                          
050005                                                                          
050105 BBB-FLYTTA-KVOI-ARS-FORBRUKN SECTION.                                    
050205                                                                          
050305     MOVE 1 TO PER-IX                                                     
050405     PERFORM UNTIL PER-IX > 12                                            
050505        MOVE OIHD-KVOI(4, PER-IX)       TO                                
050605                                    OIHD-KVOI(5, PER-IX)                  
050705                                    L411-VALUE                            
051005        MOVE OIHD-KVOI-REFILL(4, PER-IX)       TO                         
051105                                    OIHD-KVOI-REFILL(5,    PER-IX)        
051205                                    L411-VALUE                            
051505        MOVE OIHD-KVVIPER(4, PER-IX) TO                                   
051605                                    OIHD-KVVIPER(5, PER-IX)               
051705                                    L411-VALUE                            
052005        MOVE OIHD-KVOI(3, PER-IX)       TO                                
052105                                    OIHD-KVOI(4, PER-IX)                  
052205                                    L411-VALUE                            
052505        MOVE OIHD-KVOI-REFILL(3, PER-IX)       TO                         
052605                                    OIHD-KVOI-REFILL(4,    PER-IX)        
052705                                    L411-VALUE                            
053005        MOVE OIHD-KVVIPER(3, PER-IX) TO                                   
053105                                    OIHD-KVVIPER(4, PER-IX)               
053205                                    L411-VALUE                            
053505        MOVE OIHD-KVOI(2, PER-IX)       TO                                
053605                                    OIHD-KVOI(3, PER-IX)                  
053705                                    L411-VALUE                            
054005        MOVE OIHD-KVOI-REFILL(2, PER-IX)       TO                         
054105                                    OIHD-KVOI-REFILL(3,    PER-IX)        
054205                                    L411-VALUE                            
054505        MOVE OIHD-KVVIPER(2, PER-IX) TO                                   
054605                                    OIHD-KVVIPER(3, PER-IX)               
054705                                    L411-VALUE                            
055005        MOVE OIHD-KVOI(1, PER-IX)       TO                                
055105                                     OIHD-KVOI(2, PER-IX)                 
055205                                    L411-VALUE                            
055505        MOVE OIHD-KVOI-REFILL(1, PER-IX)       TO                         
055605                                    OIHD-KVOI-REFILL(2,    PER-IX)        
055705                                    L411-VALUE                            
056005        MOVE OIHD-KVVIPER(1, PER-IX) TO                                   
056105                                    OIHD-KVVIPER(2, PER-IX)               
056205                                    L411-VALUE                            
056505        ADD 1 TO PER-IX                                                   
056605     END-PERFORM                                                          
056705                                                                          
056805                                                                          
056905     MOVE 1 TO PER-IX                                                     
057005     MOVE 1 TO RULL-IX                                                    
057105     MOVE DAGENS-DATUM-AAR  TO WS-PERIOD-AA                               
057205     PERFORM UNTIL PER-IX > 12                                            
057305        MOVE ZERO         TO WS-KVVIPER                                   
057405                             WS-KVOI                                      
057505                             WS-KVOI-REFILL                               
057605        MOVE PER-IX  TO WS-PERIOD-RP                                      
057705        MOVE WS-PERIOD-AARP TO DAT-I-TIDATUM                              
057805        MOVE 'AARP  '       TO DAT-KDDATFORM                              
057905        CALL WDATKONV USING DAT-KDDATFORM                                 
058005                            DAT-I-TIDATUM                                 
058105                            DAT-O-TIDATUM                                 
058205                            DAT-KDSVAR                                    
058305       IF DAT-KDSVAR-OK                                                   
058405         MOVE DAT-KVVIPER  TO WS-KVVIPER                                  
058505       ELSE                                                               
058605         MOVE 'FEL I DATKONV1 BBB SECTION' TO FELTEXT-STR                 
058705         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
058805       END-IF                                                             
058905                                                                          
059005       COMPUTE RULL-IX-MAX = RULL-IX + WS-KVVIPER - 1                     
059105       PERFORM UNTIL RULL-IX > RULL-IX-MAX                                
059205          COMPUTE WS-KVOI = WS-KVOI + DC-KVOI-RULL(RULL-IX)               
059305          COMPUTE WS-KVOI-REFILL =                                        
059405                  WS-KVOI-REFILL + DC-KVOI-REF-RULL(RULL-IX)              
059505          ADD 1 TO RULL-IX                                                
059605       END-PERFORM                                                        
059705                                                                          
059805       MOVE WS-KVOI      TO OIHD-KVOI(1, PER-IX)                          
059905                            L411-VALUE                                    
060005       MOVE WS-KVOI-REFILL TO OIHD-KVOI-REFILL(1, PER-IX)                 
060105                              L411-VALUE                                  
060205       MOVE WS-KVVIPER   TO OIHD-KVVIPER(1, PER-IX)                       
060305                            L411-VALUE                                    
060405       ADD 1 TO PER-IX                                                    
060505     END-PERFORM                                                          
060605     .                                                                    
060705     EJECT                                                                
060805                                                                          
060905                                                                          
061005 X-TAG-CHECKPOINT   SECTION.                                              
061105                                                                          
061205     MOVE ART-IDARTNR TO W-IDARTNR                                        
061305     PERFORM IMS-CHECKPOINT                                               
061405     MOVE ZERO TO CHKP-ANT                                                
061505     PERFORM IMS-GU-WDL701                                                
061605     .                                                                    
061705     EJECT                                                                
061805* --- IMS SEKTIONER ---                                                   
061905     SKIP3                                                                
062005     EJECT                                                                
062105 IMS-GN-WDL701   SECTION.                                                 
062205                                                                          
062305     MOVE 'WDL701' TO SSA1                                                
062405     MOVE '  GEGB' TO GODK-STATUSKODER                                    
062505     CALL CBLTDLI USING GN WDL7-PCB DLI-IO-L701 SSA1                      
062605     MOVE WDL7-STATUS-CODE TO STATUS-WS                                   
062705     PERFORM IMS-STATUSKONTROLL                                           
062805     .                                                                    
062905 IMS-GU-WDL701   SECTION.                                                 
063005                                                                          
063105     STRING 'WDL701  (IDARTNR  =' W-IDARTNR-X ')'                         
063205          DELIMITED BY SIZE INTO SSA1                                     
063305     MOVE '  ' TO GODK-STATUSKODER                                        
063405     CALL CBLTDLI USING GU WDL7-PCB DLI-IO-L701 SSA1                      
063505     MOVE WDL7-STATUS-CODE TO STATUS-WS                                   
063605     PERFORM IMS-STATUSKONTROLL                                           
063705     .                                                                    
063805     EJECT                                                                
063905 IMS-GHNP-WDL711  SECTION.                                                
064005                                                                          
064105     MOVE 'WDL711' TO SSA1                                                
064205     MOVE '  GE' TO GODK-STATUSKODER                                      
064305     CALL CBLTDLI USING GHNP WDL7-PCB DLI-IO-L711 SSA1                    
064405     MOVE WDL7-STATUS-CODE TO STATUS-WS                                   
064505     PERFORM IMS-STATUSKONTROLL                                           
064605     .                                                                    
064705     SKIP3                                                                
064805 IMS-REPL-WDL711 SECTION.                                                 
064905                                                                          
065005     MOVE '  ' TO GODK-STATUSKODER                                        
065105     CALL CBLTDLI USING REPL WDL7-PCB DLI-IO-L711                         
065205     MOVE WDL7-STATUS-CODE TO STATUS-WS                                   
065305     PERFORM IMS-STATUSKONTROLL                                           
065405     .                                                                    
065505     EJECT                                                                
065605 IMS-GHU-WDL411  SECTION.                                                 
065705                                                                          
065805     STRING 'WDL401  (IDARTNR  =' W-IDARTNR-X ')'                         
065905          DELIMITED BY SIZE INTO SSA1                                     
066005     STRING 'WDL411  (IDDC     =' W-IDDC-X ')'                            
066105          DELIMITED BY SIZE INTO SSA2                                     
066205     MOVE '  GE' TO GODK-STATUSKODER                                      
066305     CALL CBLTDLI USING GHU WDL4-PCB DLI-IO-L411 SSA1 SSA2                
066405     MOVE WDL4-STATUS-CODE TO STATUS-WS                                   
066505     PERFORM IMS-STATUSKONTROLL                                           
066605     .                                                                    
066705     SKIP3                                                                
066805 IMS-REPL-WDL411 SECTION.                                                 
066905                                                                          
067005     MOVE '  ' TO GODK-STATUSKODER                                        
067105     CALL CBLTDLI USING REPL WDL4-PCB DLI-IO-L411                         
067205     MOVE WDL4-STATUS-CODE TO STATUS-WS                                   
067305     PERFORM IMS-STATUSKONTROLL                                           
067405     .                                                                    
067508     SKIP2                                                                
067606 IMS-ISRT-WDL401 SECTION.                                                 
067706                                                                          
067806     MOVE 'WDL401' TO SSA1                                                
067906     MOVE '  II'   TO GODK-STATUSKODER                                    
068006     CALL CBLTDLI USING ISRT WDL4-PCB DLI-IO-L401 SSA1                    
068106     MOVE WDL4-STATUS-CODE TO STATUS-WS                                   
068206     PERFORM IMS-STATUSKONTROLL                                           
068306     .                                                                    
068408     SKIP2                                                                
068506 IMS-ISRT-WDL411 SECTION.                                                 
068606                                                                          
068607     STRING 'WDL401  (IDARTNR  =' W-IDARTNR-X ')'                         
068608          DELIMITED BY SIZE INTO SSA1                                     
068706     MOVE 'WDL411' TO SSA2                                                
068806     MOVE '  '     TO GODK-STATUSKODER                                    
068906     CALL CBLTDLI USING ISRT WDL4-PCB DLI-IO-L411 SSA1 SSA2               
069006     MOVE WDL4-STATUS-CODE TO STATUS-WS                                   
069106     PERFORM IMS-STATUSKONTROLL                                           
069206     .                                                                    
069306     EJECT                                                                
069406 IMS-RESTART SECTION.                                                     
069506     SKIP2                                                                
069606     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
069706     MOVE '  ' TO GODK-STATUSKODER                                        
069806     CALL CBLTDLI USING XRST MSG-PCB                                      
069906                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
070006                        CHKP-AREA-LENGTH CHKP-AREA                        
070106     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
070206     PERFORM IMS-STATUSKONTROLL                                           
070306     .                                                                    
070406     EJECT                                                                
070506 IMS-CHECKPOINT SECTION.                                                  
070606     SKIP2                                                                
070706     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
070806     MOVE '  XD' TO GODK-STATUSKODER                                      
070906     CALL CBLTDLI USING CHKP MSG-PCB                                      
071006                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
071106                        CHKP-AREA-LENGTH CHKP-AREA                        
071206     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
071306     PERFORM IMS-STATUSKONTROLL                                           
071406                                                                          
071506     IF IMS-EJ-OK                                                         
071606       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
071706       DISPLAY FELTEXT                                                    
071806       CALL FELLOG                                                        
071906     END-IF                                                               
072006     .                                                                    
072106     EJECT                                                                
072206 IMS-STATUSKONTROLL SECTION.                                              
072306     SKIP2                                                                
072406     SET STATUS-IX TO 1                                                   
072506     SEARCH GODK-STATUS                                                   
072606       AT END                                                             
072706         STRING 'OTILLÅTEN RETURKOD FRÅN IMS: ' STATUS-WS                 
072806          DELIMITED BY SIZE INTO FELTEXT-STR                              
072906         DISPLAY FELTEXT                                                  
073006         CALL FELLOG                                                      
073106       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
073206         CONTINUE                                                         
074006     END-SEARCH                                                           
080004     .                                                                    
