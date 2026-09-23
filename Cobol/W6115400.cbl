000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.         W6115400.                                            
000400 AUTHOR.             STEFAN ÅSGÅRDEN.                                     
000500 DATE-WRITTEN.       JULI 2002.                                           
000600     SKIP2                                                                
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNCTION.                                                            
001010*        PROGRAMMET ÄR ETT SUBPROGRAM FÖR BERÄKNING AV                    
001100*        NETTO-BEHOV FÖR CROSSDOCKING OMRÅDEN                             
001200*        FÖR EN SPECIFIK ARTIKEL                                          
002000*    SUBPROGRAM.                                                          
002100*            W009VADD    ADD AV VECKOR TILL DATUM                         
002200*            PERADD      ADD AV PERIODER TILL DATUM                       
002400     EJECT                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP1                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000 DATA DIVISION.                                                           
003100 FILE SECTION.                                                            
003200     EJECT                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400     SKIP2                                                                
003500 77  IDPGM                       PIC X(08)   VALUE 'W6115400'.            
003600     SKIP2                                                                
004300 01  RKOD                    PIC S9(4)   VALUE +0    COMP SYNC.           
004400     SKIP3                                                                
004500 01  KONSTANTER.                                                          
004600     03  JA                  PIC X       VALUE 'J'.                       
004700     03  NEJ                 PIC X       VALUE 'N'.                       
011500     SKIP3                                                                
011510 01  FILLER                      PIC X(16)   VALUE 'ARBETSAREOR'.         
011600 01  ARBETSAREOR.                                                         
011700     SKIP1                                                                
011750*                                                                         
011751     03 IX-K7                    PIC S9(9)   VALUE ZERO COMP-3.           
011752     03 IX-K7-MAX                PIC S9(9)   VALUE ZERO COMP-3.           
011760     03 WS-K7 OCCURS 15.                                                  
011761        05 WS-K7-IDDC            PIC  X(2)   VALUE SPACE.                 
011770        05 WS-K7-CD-OMRADE       PIC S9(3)   VALUE ZERO COMP-3.           
011771        05 WS-K7-KVDAGAR-BEHOV   PIC S9(3)   VALUE ZERO COMP-3.           
011772        05 WS-K7-FL-CD-RELEASE   PIC  X(1)   VALUE SPACE.                 
011773     03  WS-ANTAL-VECKOR         PIC  9(3)   VALUE ZERO COMP-3.           
011774     03  WS-TIAAVV-NUM           PIC  9(4)   VALUE ZERO.                  
011775     03  WS-CD-TILLGANG          PIC S9(7)   VALUE ZERO COMP-3.           
011791     SKIP3                                                                
011800     03  IX-CD-OMR               PIC S9(9)   VALUE ZERO COMP-3.           
011900     03  IX-IDDC                 PIC S9(9)   VALUE ZERO COMP-3.           
027291                                                                          
027292 01  FILLER                      PIC X(16)   VALUE 'ARBETSFAELT'.         
027293 01  ARBETSFAELT.                                                         
027294*                                                                         
027295     03 DAGENS-DATUM.                                                     
027296         05 DAGENS-AAR           PIC 9(4).                                
027297         05 DAGENS-MAANAD        PIC 9(2).                                
027298         05 DAGENS-DAG           PIC 9(2).                                
027299                                                                          
027300 77  SW-TRAEFF                   PIC X       VALUE 'J'.                   
027301     88  SW-TRAEFF-JA                        VALUE 'J'.                   
027310     88  SW-TRAEFF-NEJ                       VALUE 'N'.                   
027400                                                                          
031300                                                                          
031400*      --- VALID IDDC CODES                                               
031500*                                                                         
031600*01    -COPY WWDC99                                                       
031900                                                                          
032000 01  W-IDDC-X.                                                            
032100     03  W-IDDC              PIC  X(2).                                   
032200 01  W-IDARTNR-X.                                                         
032300     03  W-IDARTNR           PIC S9(9)               COMP-3.              
032400 01  W-KDERS-0-X.                                                         
032500     03  W-KDERS-0           PIC S9(3)   COMP-3  VALUE ZERO.              
032600                                                                          
032700     SKIP2                                                                
032800 01  FELTEXT.                                                             
032900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
033000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
033100 01  FELTEXT2.                                                            
033200     03  FILLER                  PIC X(9)    VALUE 'FELTEXT2'.            
033300     03  FELTEXT2-STR      PIC X(71)   VALUE SPACE.                       
033400                                                                          
033700     EJECT                                                                
033800 01  DYNAMISKA-SUBPROGRAM.                                                
033900     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
034000     03  W009VADD            PIC X(8)    VALUE 'W009VADD'.                
034100     03  PERADD              PIC X(8)    VALUE 'PERADD'.                  
034500     03  FELLOG              PIC X(8)    VALUE 'FELLOG  '.                
034600     03  WDATKONV            PIC X(8)    VALUE 'WDATKONV'.                
034610     03  WDAGKONV            PIC X(8)    VALUE 'WDAGKONV'.                
034620     03  ABEND               PIC X(8)    VALUE 'ABEND'.                   
034630     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
034640     03  WORKDAY             PIC X(8)    VALUE 'WORKDAY'.                 
034650     03  W61158              PIC X(8)    VALUE 'W61158'.                  
035000     EJECT                                                                
035100*    ---PARAMETRAR TILL DATKONV                                           
035200*01  -COPY WDATAREA                                                       
035210     EJECT                                                                
035860*    ---PARAMETRAR TILL WORKDAY                                           
035870*01  -COPY WORKAREA                                                       
035871     EJECT                                                                
035872 01  FILLER              PIC X(8)    VALUE 'W61158  '.                    
035873*01  AREA  -COPY W61158      -PRE W61158-.                                
035880     EJECT                                                                
035920*    --- PARAMETRAR TILL ABEND                                            
035930                                                                          
035940 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
035950 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
035996     SKIP3                                                                
036005     SKIP2                                                                
036010*        ARBETSAREOR TILL IMS-SEKTIONERNA                                 
036100*                                                                         
036200 01      IMS-WS.                                                          
036300   03    FILLER          PIC X(8)    VALUE 'IMS-WS  '.                    
036400     SKIP3                                                                
036500*                            *** STATUSKOD FRÅN IMS                       
036600   03    STATUS-WS       PIC XX.                                          
036700     88  SEGMENT-FINNS               VALUE '  '.                          
036800     88  SEGMENT-SAKNAS              VALUE 'GE'                           
036900                                           'GB'.                          
037000     SKIP3                                                                
037100   03    SSA1            PIC X(128).                                      
037200   03    SSA2            PIC X(128).                                      
037300   03    SSA3            PIC X(128).                                      
037400     SKIP3                                                                
037420 01  NYCKLAR-TILL-DLI.                                                    
037430     03  W-WDGXKEY-X.                                                     
037440         05  W-IDHTYP           PIC X(4)    VALUE '2501'.                 
037450         05  W-IDDC-2501        PIC X(2)    VALUE ZERO.                   
037460         05  W-LOWVALUE         PIC X(24)   VALUE LOW-VALUE.              
037470*                                                                         
037480     03  W-IDREFTAB-X.                                                    
037490         05  W-IDREFTAB         PIC X(1)    VALUE SPACE.                  
037500   03    GODK-STATUSKODER.                                                
037600     05  GODK-STATUS OCCURS 10 INDEXED BY STATUS-IX PIC XX.               
037700     SKIP3                                                                
037800*01      -COPY W0003                                                      
037900     EJECT                                                                
038000 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK601'.             
038100     SKIP3                                                                
038200 01  DLI-IO-AREA-WDK601.                                                  
038300*        05  -COPY WDK601                                                 
038400     EJECT                                                                
038500 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK611'.             
038600     SKIP3                                                                
038700 01  DLI-IO-AREA-WDK611.                                                  
038800*        05  -COPY WDK611                                                 
040900     EJECT                                                                
041000 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK701'.             
041100     SKIP3                                                                
041200 01  DLI-IO-AREA-WDK701.                                                  
041300*        05  -COPY WDK701                                                 
041400     EJECT                                                                
041500 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK711'.             
041600     SKIP3                                                                
041700 01  DLI-IO-AREA-WDK711.                                                  
041800*        05  -COPY WDK711                                                 
041900     EJECT                                                                
042100 LINKAGE SECTION.                                                         
042200     SKIP3                                                                
042300*01  AREA  -COPY W61154      -PRE LINK-.                                  
042400     EJECT                                                                
042500*01  -COPY W0008  -PRE WDK6-.                                             
042600     05  FILLER              PIC X.                                       
042700     EJECT                                                                
042800*01  -COPY W0008  -PRE WDK7-.                                             
042900     05  FILLER              PIC X.                                       
043000     EJECT                                                                
043400*01  -COPY W0008  -PRE 2501-                                              
043500     05  FILLER                  PIC X.                                   
043501     EJECT                                                                
043502*01  -COPY W0008  -PRE WDK6-2-.                                           
043503     05  FILLER              PIC X.                                       
043504     EJECT                                                                
043505*01  -COPY W0008  -PRE WDK7-2-.                                           
043506     05  FILLER              PIC X.                                       
043507     EJECT                                                                
043508*01  -COPY W0008  -PRE WDL6-                                              
043509     05  FILLER                  PIC X.                                   
043510     EJECT                                                                
043511*01  -COPY W0008  -PRE WDD7A-                                             
043512     05  FILLER                  PIC X.                                   
043513     EJECT                                                                
043514*01  -COPY W0008  -PRE WDB6-                                              
043515     05  FILLER                  PIC X.                                   
043516                                                                          
043520     EJECT                                                                
043521 PROCEDURE DIVISION USING LINK-AREA                                       
043522                          WDK6-PCB WDK7-PCB 2501-PCB                      
043523                          WDK6-2-PCB WDK7-2-PCB                           
043524                          WDL6-PCB WDD7A-PCB WDB6-PCB.                    
044000     PERFORM A-INITIERA                                                   
044010                                                                          
044020     MOVE LINK-IDARTNR TO W-IDARTNR                                       
044030     PERFORM IMS-GU-K601                                                  
044040                                                                          
044050     IF  SEGMENT-FINNS                                                    
044070                                                                          
044080       PERFORM IMS-GNP-K611                                               
044090       IF SEGMENT-FINNS                                                   
044091                                                                          
044093         PERFORM C-BERAKNA-BEHOV                                          
044094                                                                          
044097       END-IF                                                             
044100     END-IF                                                               
045400                                                                          
072800     MOVE ZERO TO RETURN-CODE                                             
072900     GOBACK                                                               
073000     .                                                                    
073100     EJECT                                                                
073200 A-INITIERA SECTION.                                                      
073300******************************************************************        
073400*                                                                *        
073500*    BERÄKNING AV START- OCH SLUT-TIDPUNKTER (ÅR OCH VECKA)      *        
073600*    FÖR BERÄKNING                                               *        
073700*    NOLLSTÄLLNING AV TABELLER                                   *        
073800*                                                                *        
073900******************************************************************        
074000     SKIP1                                                                
076000                                                                          
076800     MOVE SPACE              TO LINK-RESULTATFLT                          
077300     MOVE ZERO               TO LINK-KVBEHOV-CD (1)                       
077400                                LINK-KVBEHOV-CD (2)                       
077500                                LINK-KVBEHOV-CD (3)                       
077510                                LINK-KVBEHOV-CD (4)                       
077520     MOVE 1                  TO IX-IDDC                                   
077530     PERFORM UNTIL IX-IDDC > 20                                           
077540       MOVE ZERO             TO LINK-KVBEHOV-DC (IX-IDDC)                 
077550       ADD 1                 TO IX-IDDC                                   
077560     END-PERFORM                                                          
077712     .                                                                    
077713     EJECT                                                                
149200 C-BERAKNA-BEHOV        SECTION.                                          
149210                                                                          
149211     MOVE LINK-IDARTNR       TO W61158-IDARTNR                            
149212     MOVE LINK-TIAAVVD       TO W61158-TIAAVVD                            
149214     CALL W61158 USING W61158-AREA WDK6-PCB WDK7-PCB                      
149215                       2501-PCB WDK6-2-PCB WDK7-2-PCB                     
149217                       WDL6-PCB WDD7A-PCB WDB6-PCB                        
149218                                                                          
149219     MOVE W61158-RESULTATFLT TO LINK-RESULTATFLT                          
149221                                                                          
149229                                                                          
149230     MOVE 1                  TO IX-CD-OMR                                 
149231                                                                          
149240     PERFORM UNTIL IX-CD-OMR > 4                                          
149250     OR CLAG-ADLAGOMR-CD (IX-CD-OMR) = ZERO                               
149310*                                                                         
149320*     RÄKNA UT REFILLBEHOVET NETTO PER CROSSDOCKING OMRÅDE                
149330*     DVS LAGERSALDOT MINUS REFILLBEHOVET                                 
149340*     ANTALET DAGARS BEHOV ÄR ANGIVET PÅ WDK7                             
149350*                                                                         
149360       COMPUTE WS-CD-TILLGANG =                                           
149370               CLAG-KVLS-CD (IX-CD-OMR)                                   
149380             - CLAG-KVRESS-CD (IX-CD-OMR)                                 
149383       COMPUTE LINK-KVBEHOV-CD (IX-CD-OMR) =                              
149384               W61158-KVBEHOV-CD (IX-CD-OMR) - WS-CD-TILLGANG             
149386       IF LINK-KVBEHOV-CD (IX-CD-OMR) < ZERO                              
149387         MOVE ZERO           TO LINK-KVBEHOV-CD (IX-CD-OMR)               
149388       END-IF                                                             
149394                                                                          
149419       ADD 1                 TO IX-CD-OMR                                 
149420                                                                          
149500     END-PERFORM                                                          
152100     .                                                                    
152200     EJECT                                                                
258476                                                                          
258477                                                                          
258478                                                                          
258479* --- IMS SEKTIONER ---                                                   
258480     SKIP3                                                                
258481                                                                          
258492     EJECT                                                                
258493                                                                          
258495 IMS-GU-K601 SECTION.                                                     
258496     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X                             
258497                    '&KDERS    =' W-KDERS-0-X ')'                         
258499            DELIMITED BY SIZE INTO SSA1                                   
258500     MOVE '  GE' TO GODK-STATUSKODER                                      
258501     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK601 SSA1               
258510     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
258600     PERFORM IMS-STATUSKONTROLL                                           
258700     .                                                                    
258800     SKIP3                                                                
258900 IMS-GNP-K611    SECTION.                                                 
259000     MOVE  'WDK611   '  TO SSA1                                           
259100     MOVE '  ' TO GODK-STATUSKODER                                        
259200     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA-WDK611 SSA1              
259300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
259400     PERFORM IMS-STATUSKONTROLL                                           
259500     .                                                                    
265000     EJECT                                                                
266800 IMS-STATUSKONTROLL SECTION.                                              
266900     SET STATUS-IX TO 1                                                   
267000     SEARCH GODK-STATUS  AT END CALL FELLOG                               
267100     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
267200          CONTINUE                                                        
267300     END-SEARCH                                                           
267400     .                                                                    
