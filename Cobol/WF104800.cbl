000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.    WF104800.                                                 
000400                                                                          
000500*    AUTHOR.        ANDERS HENRIKSSON.                                    
000600*    DATE-WRITTEN   2022-02-18.                                           
000700*                                                                         
000800*    FUNKTION:                                                            
000900*               CHANGE FILE TO DISPLAYFORMAT                              
001000*               FILES TO DATA LAKE                                        
001200*                                                                         
001300     EJECT                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500 INPUT-OUTPUT SECTION.                                                    
001600 FILE-CONTROL.                                                            
001700                                                                          
001800     SELECT DLININ  ASSIGN       TO WF1048D1.                             
001900                                                                          
002000     SELECT DHEAIN  ASSIGN       TO WF1048D2.                             
002001                                                                          
002002     SELECT DLINUT  ASSIGN       TO WF1048D3.                             
002003                                                                          
002004     SELECT DHEAUT  ASSIGN       TO WF1048D4.                             
002100     EJECT                                                                
002200                                                                          
002300 DATA DIVISION.                                                           
002400 FILE SECTION.                                                            
002500                                                                          
002600 FD  DLININ                                                               
002700     RECORDING V                                                          
002800     BLOCK CONTAINS 0.                                                    
002900                                                                          
003000 01  INTRANS1     PIC X(807).                                             
003102     SKIP2                                                                
003200                                                                          
003201 FD  DHEAIN                                                               
003202     RECORDING V                                                          
003203     BLOCK CONTAINS 0.                                                    
003204                                                                          
003205 01  INTRANS2     PIC X(1526).                                            
003208     SKIP2                                                                
003209                                                                          
003300 FD  DLINUT                                                               
003400     RECORDING F                                                          
003500     BLOCK CONTAINS 0.                                                    
003600                                                                          
003700*01  UTTRANS1     -COPY WF10DLIN -L                                       
003800     SKIP2                                                                
003801                                                                          
003802 FD  DHEAUT                                                               
003803     RECORDING F                                                          
003804     BLOCK CONTAINS 0.                                                    
003805                                                                          
003806*01  UTTRANS2     -COPY WF10DHEA -L                                       
003807     EJECT                                                                
003900                                                                          
004000 WORKING-STORAGE SECTION.                                                 
004100     SKIP3                                                                
004200 77  IDPGM                   PIC X(8)      VALUE 'WF104800'.              
004300 77  JA                      PIC X         VALUE 'J'.                     
004400 77  NEJ                     PIC X         VALUE 'N'.                     
004500 77  EOF-DLININ              PIC X         VALUE 'N'.                     
004510 77  EOF-DHEAIN              PIC X         VALUE 'N'.                     
004600                                                                          
005500 01  SUBPROGRAM.                                                          
005600     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
005700                                                                          
005800 01  DLININ-TRANSID.                                                      
005900     03  FILLER              PIC X(6) VALUE 'WF1048'.                     
006000     03  FILLER              PIC X(8) VALUE 'WF1048D1'.                   
006100     03  FILLER              PIC X(4) VALUE 'DLIN'.                       
006200                                                                          
006201 01  DHEAIN-TRANSID.                                                      
006202     03  FILLER              PIC X(6) VALUE 'WF1048'.                     
006203     03  FILLER              PIC X(8) VALUE 'WF1048D2'.                   
006204     03  FILLER              PIC X(4) VALUE 'DHEA'.                       
006205                                                                          
006300     EJECT                                                                
006400*   -COPY W0005  -PRE POSTSUM-                                            
006500     EJECT                                                                
006600                                                                          
006800 01  INAREA1.                                                             
006810     03 PRE-DLIN              PIC X(6).                                   
006900     03  -COPY T01DLIN    -PRE IN1-                                       
007000     EJECT                                                                
007010                                                                          
007020 01  INAREA2.                                                             
007021     03 PRE-DHEA              PIC X(6).                                   
007022     03  -COPY T01DHEA    -PRE IN2-                                       
007023     EJECT                                                                
007024                                                                          
007025 01  UTAREA1.                                                             
010600     03  -COPY WF10DLIN -PRE UT1-                                         
010700     SKIP2                                                                
010800                                                                          
010801 01  UTAREA2.                                                             
010802     03  -COPY WF10DHEA -PRE UT2-                                         
010803     SKIP2                                                                
010804                                                                          
010805 LINKAGE SECTION.                                                         
010806 PROCEDURE DIVISION.                                                      
010900 MAIN SECTION.                                                            
011000                                                                          
011100     PERFORM A-INIT                                                       
011200                                                                          
011300     PERFORM S01-READ-DLININ-POST                                         
011400     PERFORM UNTIL EOF-DLININ = JA                                        
011401       PERFORM B-CREATE-DLIN-POST                                         
011402       PERFORM S02-WRITE-DLINUT                                           
011403       PERFORM S01-READ-DLININ-POST                                       
011900     END-PERFORM                                                          
012000                                                                          
012100     PERFORM S01-READ-DHEAIN-POST                                         
012200     PERFORM UNTIL EOF-DHEAIN = JA                                        
012201       PERFORM B-CREATE-DHEA-POST                                         
012202       PERFORM S02-WRITE-DHEAUT                                           
012203       PERFORM S01-READ-DHEAIN-POST                                       
012204     END-PERFORM                                                          
012205                                                                          
012206     PERFORM Z-END                                                        
012207                                                                          
012300     MOVE ZERO TO RETURN-CODE                                             
012400     GOBACK                                                               
012500     .                                                                    
012600     EJECT                                                                
012700                                                                          
012800 A-INIT SECTION.                                                          
012900     OPEN INPUT  DLININ                                                   
012910     OPEN INPUT  DHEAIN                                                   
013000     OPEN OUTPUT DLINUT                                                   
013010     OPEN OUTPUT DHEAUT                                                   
013100                                                                          
013200     MOVE IDPGM           TO POSTSUM-PROGNAMN                             
013400     .                                                                    
013500     EJECT                                                                
013600                                                                          
013700 B-CREATE-DLIN-POST SECTION.                                              
013813     MOVE IN1-IDLEGSEL         TO UT1-IDLEGSEL                            
013814     MOVE IN1-DAEXDAT          TO UT1-DAEXDAT                             
013815     MOVE IN1-TIEXTID          TO UT1-TIEXTID                             
013816     MOVE IN1-KDVALISO         TO UT1-KDVALISO                            
013817     MOVE IN1-IDLANDX3-SEND    TO UT1-IDLANDX3-SEND                       
013818     MOVE IN1-IDLEVNR          TO UT1-IDLEVNR                             
013819     MOVE IN1-IDPARTNR         TO UT1-IDPARTNR                            
013820     MOVE IN1-KDFINDOC         TO UT1-KDFINDOC                            
013821     MOVE IN1-FLSOFT           TO UT1-FLSOFT                              
013822     MOVE IN1-FLFREE           TO UT1-FLFREE                              
013823     MOVE IN1-FLPRIV           TO UT1-FLPRIV                              
013824     MOVE IN1-IDBREAK-1        TO UT1-IDBREAK(1)                          
013825     MOVE IN1-IDBREAK-2        TO UT1-IDBREAK(2)                          
013826     MOVE IN1-IDLOPNR          TO UT1-IDLOPNR                             
013827     MOVE IN1-IDLANDX3-REC     TO UT1-IDLANDX3-REC                        
013828     MOVE IN1-IDEXCUST-1       TO UT1-IDEXCUST(1)                         
013829     MOVE IN1-IDEXCUST-2       TO UT1-IDEXCUST(2)                         
013830     MOVE IN1-IDEXCUST-3       TO UT1-IDEXCUST(3)                         
013831     MOVE IN1-IDBUNDLE         TO UT1-IDBUNDLE                            
013832     MOVE IN1-IDREF            TO UT1-IDREF                               
013833     MOVE IN1-IDREFRAD         TO UT1-IDREFRAD                            
013834     MOVE IN1-DAREFDAT         TO UT1-DAREFDAT                            
013835     MOVE IN1-BEVOLREF         TO UT1-BEVOLREF                            
013836     MOVE IN1-IDOPTION-1       TO UT1-IDOPTION(1)                         
013837     MOVE IN1-IDOPTION-2       TO UT1-IDOPTION(2)                         
013838     MOVE IN1-IDOPTION-3       TO UT1-IDOPTION(3)                         
013839     MOVE IN1-IDOPTION-4       TO UT1-IDOPTION(4)                         
013840     MOVE IN1-IDOPTION-5       TO UT1-IDOPTION(5)                         
013841     MOVE IN1-IDARTNR-FINANCE  TO UT1-IDARTNR-FINANCE                     
013842     MOVE IN1-BEART            TO UT1-BEART                               
013843     MOVE IN1-IDSTATNR         TO UT1-IDSTATNR                            
013844     MOVE IN1-VKORDBTO-KOLLI   TO UT1-VKORDBTO-KOLLI                      
013845     MOVE IN1-VKARTNTO         TO UT1-VKARTNTO                            
013846     MOVE IN1-KDARTURS         TO UT1-KDARTURS                            
013847     MOVE IN1-KVBEART          TO UT1-KVBEART                             
013848     MOVE IN1-KVLEVART         TO UT1-KVLEVART                            
013849     MOVE IN1-PRARTBTO         TO UT1-PRARTBTO                            
013850     MOVE IN1-PRARTNTO         TO UT1-PRARTNTO                            
013851     MOVE IN1-REARTRAB         TO UT1-REARTRAB                            
013852     MOVE IN1-KDVAT            TO UT1-KDVAT                               
013853     MOVE IN1-FLSPECPR         TO UT1-FLSPECPR                            
013854     MOVE IN1-KDANMORS         TO UT1-KDANMORS                            
013855     MOVE IN1-IDFAKREF         TO UT1-IDFAKREF                            
013856     MOVE IN1-DAFAKREF         TO UT1-DAFAKREF                            
013857     MOVE IN1-IDDC             TO UT1-IDDC                                
013858     MOVE IN1-KDFRAKT          TO UT1-KDFRAKT                             
013859     MOVE IN1-BELEVVIL         TO UT1-BELEVVIL                            
013860     MOVE IN1-IDACCNT-1        TO UT1-IDACCNT(1)                          
013861     MOVE IN1-IDACCNT-2        TO UT1-IDACCNT(2)                          
013862     MOVE IN1-IDACCNT-3        TO UT1-IDACCNT(3)                          
013863     MOVE IN1-IDACCNT-4        TO UT1-IDACCNT(4)                          
013864*    MOVE SPACE                TO UT1-FILLER                              
013865     MOVE IN1-SUNTO            TO UT1-SUNTO                               
013866     MOVE IN1-REVAT            TO UT1-REVAT                               
013867     MOVE IN1-SUVAT-BILLIT     TO UT1-SUVAT-BILLIT                        
013868     MOVE IN1-SUBTO            TO UT1-SUBTO                               
013869     MOVE IN1-BEVAT            TO UT1-BEVAT                               
013870     MOVE IN1-IDAPPEND         TO UT1-IDAPPEND                            
013871     MOVE IN1-IDARTNR-CNTRL    TO UT1-IDARTNR-CNTRL                       
013872     MOVE IN1-FLPCOO           TO UT1-FLPCOO                              
013873     MOVE IN1-IDLEVNR-ART      TO UT1-IDLEVNR-ART                         
013874     IF IN1-IDTRACK-1 > SPACE                                             
013875       MOVE IN1-IDTRACK-1      TO UT1-IDTRACK(1)                          
013876     ELSE                                                                 
013877       MOVE SPACE              TO UT1-IDTRACK(1)                          
013878     END-IF                                                               
013879     IF IN1-IDTRACK-2 > SPACE                                             
013880       MOVE IN1-IDTRACK-2      TO UT1-IDTRACK(2)                          
013881     ELSE                                                                 
013882       MOVE SPACE              TO UT1-IDTRACK(2)                          
013883     END-IF                                                               
013884     IF IN1-IDTRACK-3 > SPACE                                             
013885       MOVE IN1-IDTRACK-3      TO UT1-IDTRACK(3)                          
013886     ELSE                                                                 
013887       MOVE SPACE              TO UT1-IDTRACK(3)                          
013888     END-IF                                                               
013889     IF IN1-IDTRACK-4 > SPACE                                             
013890       MOVE IN1-IDTRACK-4      TO UT1-IDTRACK(4)                          
013891     ELSE                                                                 
013892       MOVE SPACE              TO UT1-IDTRACK(4)                          
013893     END-IF                                                               
013894     IF IN1-IDTRACK-5 > SPACE                                             
013895       MOVE IN1-IDTRACK-5      TO UT1-IDTRACK(5)                          
013896     ELSE                                                                 
013897       MOVE SPACE              TO UT1-IDTRACK(5)                          
013898     END-IF                                                               
013899     IF IN1-KVANT-TRACK-1 NUMERIC                                         
013900       MOVE IN1-KVANT-TRACK-1  TO UT1-KVANT-TRACK(1)                      
013901     ELSE                                                                 
013902       MOVE ZERO               TO UT1-KVANT-TRACK(1)                      
013903     END-IF                                                               
013904     IF IN1-KVANT-TRACK-2 NUMERIC                                         
013905       MOVE IN1-KVANT-TRACK-2  TO UT1-KVANT-TRACK(2)                      
013906     ELSE                                                                 
013907       MOVE ZERO               TO UT1-KVANT-TRACK(2)                      
013908     END-IF                                                               
013909     IF IN1-KVANT-TRACK-3 NUMERIC                                         
013910       MOVE IN1-KVANT-TRACK-3  TO UT1-KVANT-TRACK(3)                      
013920     ELSE                                                                 
014000       MOVE ZERO               TO UT1-KVANT-TRACK(3)                      
014100     END-IF                                                               
014200     IF IN1-KVANT-TRACK-4 NUMERIC                                         
014300       MOVE IN1-KVANT-TRACK-4  TO UT1-KVANT-TRACK(4)                      
014310     ELSE                                                                 
014320       MOVE ZERO               TO UT1-KVANT-TRACK(4)                      
014330     END-IF                                                               
014340     IF IN1-KVANT-TRACK-5 NUMERIC                                         
014350       MOVE IN1-KVANT-TRACK-5  TO UT1-KVANT-TRACK(5)                      
014360     ELSE                                                                 
014370       MOVE ZERO               TO UT1-KVANT-TRACK(5)                      
014380     END-IF                                                               
014400     .                                                                    
014500     EJECT                                                                
014600                                                                          
014601 B-CREATE-DHEA-POST SECTION.                                              
014602     MOVE IN2-IDLEGSEL         TO UT2-IDLEGSEL                            
014603     MOVE IN2-DAEXDAT          TO UT2-DAEXDAT                             
014604     MOVE IN2-TIEXTID          TO UT2-TIEXTID                             
014605     MOVE IN2-KDVALISO         TO UT2-KDVALISO                            
014606     MOVE IN2-IDLANDX3-SEND    TO UT2-IDLANDX3-SEND                       
014607     MOVE IN2-IDLEVNR          TO UT2-IDLEVNR                             
014608     MOVE IN2-IDPARTNR         TO UT2-IDPARTNR                            
014609     MOVE IN2-KDFINDOC         TO UT2-KDFINDOC                            
014610     MOVE IN2-FLSOFT           TO UT2-FLSOFT                              
014611     MOVE IN2-FLFREE           TO UT2-FLFREE                              
014612     MOVE IN2-FLPRIV           TO UT2-FLPRIV                              
014613     MOVE IN2-IDBREAK-1        TO UT2-IDBREAK(1)                          
014614     MOVE IN2-IDBREAK-2        TO UT2-IDBREAK(2)                          
014615     MOVE IN2-IDFINDOC         TO UT2-IDFINDOC                            
014616     MOVE IN2-DAFINDOC         TO UT2-DAFINDOC                            
014617     MOVE IN2-IDSYSTEM-SEND    TO UT2-IDSYSTEM-SEND                       
014618     MOVE IN2-IDSYSTEM-REC     TO UT2-IDSYSTEM-REC                        
014619     MOVE IN2-IDSPRAK          TO UT2-IDSPRAK                             
014620     MOVE IN2-KDBETALV         TO UT2-KDBETALV                            
014621     MOVE IN2-BEBETVIL         TO UT2-BEBETVIL                            
014622     MOVE IN2-BELEGRAD-1       TO UT2-BELEGRAD(1)                         
014623     MOVE IN2-BELEGRAD-2       TO UT2-BELEGRAD(2)                         
014624     MOVE IN2-ADLEG-STREET     TO UT2-ADLEG-STREET                        
014625     MOVE IN2-ADLEG-BOX        TO UT2-ADLEG-BOX                           
014626     MOVE IN2-ADLEG-CITY       TO UT2-ADLEG-CITY                          
014627     MOVE IN2-ADLEG-PCODE      TO UT2-ADLEG-PCODE                         
014628     MOVE IN2-IDLANDX3-LEG     TO UT2-IDLANDX3-LEG                        
014629     MOVE IN2-IDTFN-LEG        TO UT2-IDTFN-LEG                           
014630     MOVE IN2-IDTFX-LEG        TO UT2-IDTFX-LEG                           
014631     MOVE IN2-IDMAIL-LEG       TO UT2-IDMAIL-LEG                          
014632     MOVE IN2-BECONT-LEG       TO UT2-BECONT-LEG                          
014633     MOVE IN2-IDVAT-LEG        TO UT2-IDVAT-LEG                           
014634     MOVE IN2-IDBG-LEG         TO UT2-IDBG-LEG                            
014635     MOVE IN2-IDPG-LEG         TO UT2-IDPG-LEG                            
014636     MOVE IN2-BERESPRA-1       TO UT2-BERESPRA(1)                         
014637     MOVE IN2-BERESPRA-2       TO UT2-BERESPRA(2)                         
014638     MOVE IN2-ADRESP-STREET    TO UT2-ADRESP-STREET                       
014639     MOVE IN2-ADRESP-BOX       TO UT2-ADRESP-BOX                          
014640     MOVE IN2-ADRESP-CITY      TO UT2-ADRESP-CITY                         
014641     MOVE IN2-ADRESP-PCODE     TO UT2-ADRESP-PCODE                        
014642     MOVE IN2-IDLANDX3-RESP    TO UT2-IDLANDX3-RESP                       
014643     MOVE IN2-IDTFN-RESP       TO UT2-IDTFN-RESP                          
014644     MOVE IN2-IDTFX-RESP       TO UT2-IDTFX-RESP                          
014645     MOVE IN2-IDMAIL-RESP      TO UT2-IDMAIL-RESP                         
014646     MOVE IN2-BECONT-RESP      TO UT2-BECONT-RESP                         
014647     MOVE IN2-IDVAT-RESP       TO UT2-IDVAT-RESP                          
014648     MOVE IN2-IDBG-RESP        TO UT2-IDBG-RESP                           
014649     MOVE IN2-IDPG-RESP        TO UT2-IDPG-RESP                           
014650     MOVE IN2-BEBET-NAME1      TO UT2-BEBET-NAME1                         
014651     MOVE IN2-BEBET-NAME2      TO UT2-BEBET-NAME2                         
014652     MOVE IN2-ADBET-STREET     TO UT2-ADBET-STREET                        
014653     MOVE IN2-ADBET-BOX        TO UT2-ADBET-BOX                           
014654     MOVE IN2-ADBET-CITY       TO UT2-ADBET-CITY                          
014655     MOVE IN2-ADBET-PCODE      TO UT2-ADBET-PCODE                         
014656     MOVE IN2-IDLANDX3-BET     TO UT2-IDLANDX3-BET                        
014657     MOVE IN2-IDVAT-BET        TO UT2-IDVAT-BET                           
014658     MOVE IN2-SUNTO-PART       TO UT2-SUNTO-PART                          
014659     MOVE IN2-SUNTO-SERV       TO UT2-SUNTO-SERV                          
014660     MOVE IN2-SUBTO-PART       TO UT2-SUBTO-PART                          
014661     MOVE IN2-SUBTO-SERV       TO UT2-SUBTO-SERV                          
014662     MOVE IN2-SUNTO-TOT        TO UT2-SUNTO-TOT                           
014663     MOVE IN2-SUVAT-BILLIT-TOT TO UT2-SUVAT-BILLIT-TOT                    
014664     MOVE IN2-SUBTO-TOT        TO UT2-SUBTO-TOT                           
014665     MOVE IN2-KDTRADP          TO UT2-KDTRADP                             
014666     MOVE IN2-PRKURS           TO UT2-PRKURS                              
014667     MOVE IN2-BEANST           TO UT2-BEANST                              
014668     MOVE IN2-IDUSER           TO UT2-IDUSER                              
014669     MOVE IN2-BETEXT-1         TO UT2-BETEXT(1)                           
014670     MOVE IN2-BETEXT-2         TO UT2-BETEXT(2)                           
014671     MOVE IN2-BETEXT-3         TO UT2-BETEXT(3)                           
014672     MOVE IN2-BETEXT-4         TO UT2-BETEXT(4)                           
014674     MOVE IN2-IDVAT-AGENT      TO UT2-IDVAT-AGENT                         
014678     MOVE IN2-BETEXT           TO UT2-BETEXT-5                            
014679     MOVE IN2-BETEXT-CRE       TO UT2-BETEXT-CRE                          
014680     .                                                                    
014681     EJECT                                                                
014690                                                                          
017200 Z-END    SECTION.                                                        
017300     CLOSE DLININ                                                         
017400           DHEAIN                                                         
017401           DLINUT                                                         
017402           DHEAUT                                                         
017500                                                                          
017600     MOVE 'S'        TO POSTSUM-OPKOD                                     
017700     CALL POSTSUM USING POSTSUM-PARM                                      
017800     .                                                                    
017900     EJECT                                                                
018000                                                                          
018100 S01-READ-DLININ-POST SECTION.                                            
018200     READ DLININ INTO INAREA1                                             
018300     AT END                                                               
018400       MOVE JA TO EOF-DLININ                                              
018500     NOT AT END                                                           
018600       MOVE DLININ-TRANSID TO POSTSUM-TRANSID                             
018700       CALL POSTSUM USING POSTSUM-PARM                                    
018800     END-READ                                                             
018900     .                                                                    
018901                                                                          
018902 S01-READ-DHEAIN-POST SECTION.                                            
018903     READ DHEAIN INTO INAREA2                                             
018904     AT END                                                               
018905       MOVE JA TO EOF-DHEAIN                                              
018906     NOT AT END                                                           
018907       MOVE DHEAIN-TRANSID TO POSTSUM-TRANSID                             
018908       CALL POSTSUM USING POSTSUM-PARM                                    
018909     END-READ                                                             
018910     .                                                                    
018911                                                                          
018912 S02-WRITE-DLINUT SECTION.                                                
018920     WRITE UTTRANS1    FROM UTAREA1                                       
018930     .                                                                    
018940                                                                          
018950 S02-WRITE-DHEAUT SECTION.                                                
018960     WRITE UTTRANS2    FROM UTAREA2                                       
018970     .                                                                    
