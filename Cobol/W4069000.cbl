000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4069000.                                                
000300 AUTHOR.         GERY CARMICHAEL.                                         
000400 DATE-WRITTEN.   99/03/26.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET ÄR EN BAKGRUNDS-MPP SOM                               
000900*        SKICKAR SOFTWARE ORDER TILL PIE                                  
001000*        VIA VCOM.                                                        
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W4T690X                                             
001400*        MID:         W4I69001                                            
001500*                                                                         
001600*    UTDATA:                                                              
001700*        UTSKRIVNA ORDER   (VIA W006PRC1)                                 
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000                                                                          
002100 DATA DIVISION.                                                           
002200                                                                          
002300     EJECT                                                                
002400 WORKING-STORAGE SECTION.                                                 
002500                                                                          
002600*    -- CHECKED BY WY2000                                                 
002700                                                                          
002800 77  IDPGM                       PIC X(08)   VALUE 'W4069000'.            
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  NEJ                         PIC X       VALUE 'N'.                   
003500                                                                          
003600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
003700     88  EGEN-MID                            VALUE '4690'.                
003800     88  GODK-MID                            VALUE '4690'                 
003900                                                   '4695'.                
004000                                                                          
004110 77  WS-IDARTNR                  PIC 9(9).                                
004200                                                                          
004210 77  WS-START-POST       PIC X(20)   VALUE '((DIS031-02-01)(CN))'.        
004220 77  WS-SLUT-POST        PIC X(6)    VALUE '((CY))'.                      
004243                                                                          
004250 01  VG5-UTAREA.                                                          
004260*    03  -COPY W463VG5      -PRE UT-                                      
004700                                                                          
006400     EJECT                                                                
040300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
040400 01  GENERELLA-SUBPROGRAM.                                                
040500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
040510     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
040600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
040800     03  W006PRC1                PIC X(8)    VALUE 'W006PRC1'.            
040900     03  W006PRT                 PIC X(8)    VALUE 'W006PRT '.            
040910     03  W009CIA                 PIC X(8)    VALUE 'W009CIA '.            
041000     EJECT                                                                
041010*    --- PARAMETRAR TILL ABEND                                            
041020                                                                          
041030 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
041040 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
041050 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
041060     SKIP2                                                                
041070 01  FELTEXT.                                                             
041080     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
041090     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
041091     EJECT                                                                
041100*    --- W006PRAR, AREA FÖR W006PRC1                                      
041200 01  FILLER                    PIC X(8)  VALUE 'W006PRAR'.                
041300                                                                          
041400*01  -COPY W006PRAR                                                       
041500     EJECT                                                                
041600 01  FILLER                    PIC X(8)  VALUE 'W006PRVC'.                
041700                                                                          
041800*01  -COPY W006PRVC                                                       
041900     EJECT                                                                
042000*01  -COPY W006PRT                                                        
042100     EJECT                                                                
042110                                                                          
042120 01  FILLER                      PIC X(16) VALUE 'CIA-AREA'.              
042130     SKIP3                                                                
042140*   -COPY W009CIA                                                         
042150*                                                                         
042160     EJECT                                                                
042200*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
042300*                                                                         
042400 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
042500     SKIP3                                                                
042600*01  MID -COPY W4I69001                                                   
042700     EJECT                                                                
042800 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
042900                                                                          
043000*01  -COPY WMSGAREA                                                       
043100     EJECT                                                                
043200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
043300                                                                          
043400*01  -COPY WMSGSPAR                                                       
043500                                                                          
043600     EJECT                                                                
043610*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
043620*                                                                         
043630     SKIP2                                                                
043640 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
043650     SKIP3                                                                
043695*    --- STATUS-KOD FRÅN IMS                                              
043696 01  STATUS-WS                   PIC XX.                                  
043697     88  SEGMENT-FINNS                       VALUE '  '.                  
043699     SKIP2                                                                
043700 01  GODK-STATUSKODER.                                                    
043701     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
043702     SKIP3                                                                
043703 01  SSA1                        PIC X(64).                               
043704     EJECT                                                                
043705*    --- IMS FUNKTIONSKODER                                               
043706*01  -COPY W0003                                                          
043707     EJECT                                                                
043900                                                                          
045600 LINKAGE SECTION.                                                         
045700                                                                          
045800*01  -COPY W0009   -PRE MSG-                                              
045900     EJECT                                                                
046000*01  -COPY W0009   -PRE ALT-                                              
046100     EJECT                                                                
046200 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB.                               
046300 MAIN SECTION.                                                            
046400     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB.                               
046500                                                                          
046600     PERFORM IMS-GET-MSG                                                  
046700     IF SEGMENT-FINNS                                                     
046800       PERFORM A-INIT                                                     
046900                                                                          
047100       MOVE 'PIE     '           TO PRT-IDPRTLST                          
047200       MOVE 001                  TO PRT-KDCALL                            
047300       CALL W006PRT USING PRT-W006PRT                                     
047400                                                                          
047500       PERFORM S01-PRT-OPEN                                               
047600                                                                          
047660       PERFORM B-SKAPA-VCOM                                               
047661                                                                          
047670       PERFORM S03-PRT-CLOSE                                              
047680                                                                          
048500     END-IF                                                               
048800                                                                          
048900     MOVE ZERO TO RETURN-CODE                                             
049000     GOBACK                                                               
049100     .                                                                    
049200                                                                          
049300     EJECT                                                                
049400 A-INIT SECTION.                                                          
049500                                                                          
049600     MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I69001                    
049700     MOVE MSG-IDTRANS-1      TO MSG-SPAR-IDTRANS                          
049800     MOVE MSG-KDMFSFOR-1     TO MSG-SPAR-KDMFSFOR                         
049900                                                                          
050000     MOVE MSG-KDTRTYP TO MSG-SPAR-KDTRTYP                                 
050100     MOVE MSG-IDPFK TO MSG-SPAR-IDPFK                                     
050200     MOVE MSG-SPAR-IDTRANS TO W-IDTRANS                                   
050300                                                                          
050400     MOVE LOW-VALUE TO MSG-AREA                                           
050510     .                                                                    
050600                                                                          
050700     EJECT                                                                
050710 B-SKAPA-VCOM SECTION.                                                    
050711     MOVE 'VG1'                     TO UT-IDPTYP                          
050720     MOVE MID-IDDISTR (2:4)         TO UT-IDDISTR                         
050730     MOVE MID-IDKUNDNR(2:6)         TO UT-IDKUNDNR                        
050740     MOVE MID-IDORDNR5              TO UT-IDORDNR7                        
050750     MOVE MID-IDPRODNR              TO UT-IDPRODNR                        
050751     MOVE MID-IDBIL                 TO UT-IDBIL                           
050761     MOVE MID-KVBEART               TO UT-KVANTAL                         
050762     MOVE 'VG2'                     TO UT-IDPTYP2                         
050770                                                                          
050780     MOVE 'VO '                     TO CIA-IDARTPRE-IN                    
050790     MOVE MID-IDARTNR               TO CIA-IDARTBET-IN                    
050791                                       WS-IDARTNR                         
050792     CALL W009CIA USING CIA-W009CIA                                       
050793     IF CIA-KDSVAR NOT = 'F'                                              
050794        MOVE CIA-IDARTPRE-UT        TO UT-IDARTPRE                        
050795        MOVE CIA-IDARTBET-UT        TO UT-IDARTBET                        
050796     ELSE                                                                 
050797       CONTINUE                                                           
050798       MOVE MID-IDARTNR             TO WS-IDARTNR                         
050799       STRING 'ARTIKEL ' WS-IDARTNR ' FUNKAR EJ I W009CIA'                
050800       DELIMITED BY SIZE INTO FELTEXT-STR                                 
050801       MOVE +37                     TO RKOD-ABEND                         
050802       CALL ABEND USING RKOD-ABEND                                        
050803     END-IF                                                               
050804                                                                          
050805     MOVE MID-IDPURAD               TO UT-IDRADNR                         
050900                                                                          
051000     MOVE PRT-NYSIDA-RAD1           TO PRT-RADSKIP                        
051100                                                                          
051410     MOVE +20                       TO PRC1-KVLRECL                       
051500     MOVE WS-START-POST             TO PRC1-DATA                          
051600     CALL W006PRC1 USING               PRT-VCOM                           
051700                                       PRT-WRITE                          
051800                                       PRT-IDPRTLST                       
051900                                       ALT-PCB                            
052000                                       PRC1-W006PRVC                      
052100     MOVE PRT-AFTER-1               TO PRT-RADSKIP                        
059520                                                                          
059530     MOVE SPACE                     TO PRC1-DATA                          
059540     MOVE VG5-UTAREA                TO PRC1-DATA                          
059541     MOVE +70                       TO PRC1-KVLRECL                       
059550     CALL W006PRC1 USING               PRT-VCOM                           
059560                                       PRT-WRITE                          
059570                                       PRT-IDPRTLST                       
059580                                       ALT-PCB                            
059590                                       PRC1-W006PRVC                      
059591     MOVE PRT-AFTER-1               TO PRT-RADSKIP                        
059602                                                                          
059603     MOVE SPACE                     TO PRC1-DATA                          
059606     MOVE WS-SLUT-POST              TO PRC1-DATA                          
059607     CALL W006PRC1 USING               PRT-VCOM                           
059608                                       PRT-WRITE                          
059609                                       PRT-IDPRTLST                       
059610                                       ALT-PCB                            
059611                                       PRC1-W006PRVC                      
059612     MOVE PRT-AFTER-1               TO PRT-RADSKIP                        
059624                                                                          
059630     .                                                                    
059800     EJECT                                                                
059900 S01-PRT-OPEN  SECTION.                                                   
060000                                                                          
060100     MOVE 'W463Z1SE'       TO    PRT-IDVCOM                               
060200     MOVE 'W463VG1 '       TO    PRT-IDCPYTXT                             
060300     MOVE +609             TO    PRC1-KVLRECL                             
060400     MOVE 'W463    '       TO    PRC1-IDVCINIT                            
060500     MOVE 'PVED4SEE'       TO    PRC1-TEVCOMST                            
060600     MOVE SPACE            TO    PRC1-DATA                                
060700                                                                          
060900     CALL W006PRC1 USING         PRT-VCOM                                 
061000                                 PRT-OPEN                                 
061100                                 PRT-IDPRTLST                             
061200                                 ALT-PCB                                  
061300                                 PRC1-W006PRVC                            
062200     .                                                                    
062300                                                                          
062400     EJECT                                                                
062500 S03-PRT-CLOSE SECTION.                                                   
062600                                                                          
062800       CALL W006PRC1 USING       PRT-VCOM                                 
062900                                 PRT-CLOSE                                
063000                                 PRT-IDPRTLST                             
063100                                 ALT-PCB                                  
063200                                 PRC1-W006PRVC                            
064100     .                                                                    
064300     EJECT                                                                
071500* --- IMS SEKTIONER ---                                                   
071700                                                                          
071800 IMS-GET-MSG SECTION.                                                     
071900                                                                          
072000     MOVE '  QC' TO GODK-STATUSKODER                                      
072100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
072200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
072300     PERFORM IMS-STATUSKONTROLL                                           
072400     .                                                                    
072800 IMS-STATUSKONTROLL SECTION.                                              
072900                                                                          
073000     SET STATUS-IX TO 1                                                   
073100     SEARCH GODK-STATUS                                                   
073200       AT END                                                             
073300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
073400         DELIMITED BY SIZE INTO FELTEXT-STR                               
073500         CALL FELLOG                                                      
073600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
073700         CONTINUE                                                         
073800     END-SEARCH                                                           
073900     .                                                                    
