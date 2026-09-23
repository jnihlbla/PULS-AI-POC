020000 ID DIVISION.                                                             
030000 PROGRAM-ID.     W5533300.                                                
040000 AUTHOR.         ARINDAM METIA.                                           
050000 DATE-WRITTEN.   24/12/01.                                                
060000 DATE-COMPILED.                                                           
070000                                                                          
080000*                                                                         
090000*    FUNCTION:                                                            
100024*        CREATE LINE DATA RECORD TO SEND TO SNOWFLAKE FOR                 
110024*        PRICE ADJUSTMENT                                                 
124024*                                                                         
130000*    ABENDCODES:                                                          
140000*        U0016 -  . . . .                                                 
150000*        U1000 -  . . . .                                                 
160000*                                                                         
170000                                                                          
180000     SKIP3                                                                
190000 ENVIRONMENT DIVISION.                                                    
200000     SKIP2                                                                
210000 INPUT-OUTPUT SECTION.                                                    
220000                                                                          
230000 FILE-CONTROL.                                                            
240100     SKIP2                                                                
240200*          --- W55324                                                     
240300     SELECT W55324                     ASSIGN TO W55333D1.                
240400     SKIP2                                                                
240500*          --- W55333                                                     
241000     SELECT W55333                     ASSIGN TO W55333D2.                
260000     EJECT                                                                
270000 DATA DIVISION.                                                           
280000     SKIP3                                                                
290000 FILE SECTION.                                                            
300100     SKIP3                                                                
300224                                                                          
300300 FD  W55324                                                               
300400     RECORDING       F                                                    
300500     BLOCK CONTAINS  0.                                                   
300600*01  -COPY WDH801      -L.                                                
300700     SKIP3                                                                
300824                                                                          
300900 FD  W55333                                                               
301000     RECORDING       F                                                    
301100     BLOCK CONTAINS  0.                                                   
302019 01  OP-RECORD                   PIC X(927).                              
310000     EJECT                                                                
311024                                                                          
320000 WORKING-STORAGE SECTION.                                                 
330000                                                                          
340000 77  IDPGM                       PIC X(8)    VALUE 'W5533300'.            
350000 77  YES                         PIC X       VALUE 'J'.                   
360000 77  NOO                         PIC X       VALUE 'N'.                   
380100                                                                          
380200 77  W55324-EOF-SW               PIC X       VALUE 'N'.                   
381000     88  END-OF-W55324                       VALUE 'J'.                   
390000     EJECT                                                                
391024                                                                          
391024 01  WS-TEMP-WEEK                PIC X(2).                                
400000 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
410000 01  FILLER REDEFINES TODAYS-DATE.                                        
420000     03  TODAYS-DATE-YEAR        PIC 9(2).                                
430000     03  TODAYS-DATE-MONTH       PIC 9(2).                                
440000     03  TODAYS-DATE-DAY         PIC 9(2).                                
441003                                                                          
441004 01  WS-DATUM.                                                            
441005     03  WS-DAGENS-DATUM         PIC 9(8).                                
450000     EJECT                                                                
460000 01  GENERAL-SUBPROGRAMS.                                                 
470000*                                                                         
471018     03  DATKORT                 PIC X(8)    VALUE 'DATKORT '.            
480000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
491000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
500000     SKIP2                                                                
500118                                                                          
501018*    --- PARAMETRAR TILL DATKORT                                          
502018*                                                                         
503018 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
504018                                                                          
505018*01  -COPY WDATKORT                                                       
506018     EJECT                                                                
507018                                                                          
510000*    --- PARAMETERS TO ABEND                                              
520000                                                                          
530000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
540000 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
550000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
560000     SKIP2                                                                
591015                                                                          
592016 01  TEMP-AREA.                                                           
600016*    03   -COPY WDH801 -PRE  TP-                                          
600017     EJECT                                                                
600018                                                                          
600116 01  WS-TP-COMMENT               PIC X(25).                               
600216 01  WS-TP-REPORT-TYPE           PIC X(10).                               
600216 01  WS-TP-REPORT-DESC           PIC X(31).                               
600216 01  WS-TP-INK-ANDEL             PIC S9(7)V9(4) COMP-3.                   
600216 01  WS-TP-STD-ANDEL             PIC S9(7)V9(4) COMP-3.                   
600216 01  WS-TP-WEEK                  PIC X(2).                                
600216 01  WS-TP-USER-ID               PIC X(20).                               
600315                                                                          
603700     EJECT                                                                
603800*    --- PARAMETRAR TILL POSTSUM                                          
603900*                                                                         
604000*01  -COPY W0005   -PRE  POSTSUM-                                         
620100     EJECT                                                                
620600 01  IN-AREA.                                                             
600016*    03   -COPY WDH801 -PRE  IN-                                          
621300     EJECT                                                                
621403                                                                          
621503 01  OP-AREA.                                                             
621624     03 WS-REPORT-TYPE           PIC X(10).                               
621724     03 FILLER                   PIC X          VALUE ';'.                
621624     03 WS-REPORT-DESC           PIC X(31).                               
621724     03 FILLER                   PIC X          VALUE ';'.                
621824     03 WS-IDARTNR               PIC Z(8)9.                               
621924     03 FILLER                   PIC X          VALUE ';'.                
622024     03 WS-DAREGDAT              PIC 9(8).                                
622124     03 FILLER                   PIC X          VALUE ';'.                
622224     03 WS-O-IDLEVNR-PR          PIC X(5).                                
622324     03 FILLER                   PIC X          VALUE ';'.                
622924     03 WS-O-KDPRURSP            PIC X.                                   
623024     03 FILLER                   PIC X          VALUE ';'.                
623124     03 WS-O-TIPRLIST            PIC Z(6)9.                               
623224     03 FILLER                   PIC X          VALUE ';'.                
623324     03 WS-O-PRARTBES-PR         PIC Z(6)9.9(2).                          
623424     03 FILLER                   PIC X          VALUE ';'.                
623524     03 WS-O-PRARTBEL-PR         PIC Z(7)9.9(5).                          
623624     03 FILLER                   PIC X          VALUE ';'.                
623724     03 WS-O-KDSTATUS-PR         PIC 9.                                   
623824     03 FILLER                   PIC X          VALUE ';'.                
623924     03 WS-O-SUINLEV-PR          PIC Z(2)9.                               
624024     03 FILLER                   PIC X          VALUE ';'.                
624124     03 WS-O-KDVALISO            PIC X(3).                                
624224     03 FILLER                   PIC X          VALUE ';'.                
624324     03 WS-O-PRARTBES            PIC Z(6)9.9(2).                          
624424     03 FILLER                   PIC X          VALUE ';'.                
624524     03 WS-O-PRARTSJK            PIC Z(6)9.9(2).                          
624624     03 FILLER                   PIC X          VALUE ';'.                
624724     03 WS-O-RETULF              PIC Z(2)9.9(4).                          
624824     03 FILLER                   PIC X          VALUE ';'.                
624924     03 WS-O-PRLFKST             PIC Z(2)9.9(2).                          
625024     03 FILLER                   PIC X          VALUE ';'.                
625124     03 WS-O-KDCMD               PIC X.                                   
625224     03 FILLER                   PIC X          VALUE ';'.                
625324     03 WS-O-PRARTSTD            PIC Z(6)9.9(2).                          
625424     03 FILLER                   PIC X          VALUE ';'.                
625524     03 WS-O-PRINK               PIC Z(6)9.9(2).                          
625624     03 FILLER                   PIC X          VALUE ';'.                
625724     03 WS-O-PRDIRLON            PIC Z(3)9.9(3).                          
625824     03 FILLER                   PIC X          VALUE ';'.                
625924     03 WS-O-PRDMTRL             PIC Z(5)9.9(3).                          
626024     03 FILLER                   PIC X          VALUE ';'.                
626124     03 WS-O-PROVRPAL            PIC Z(3)9.9(3).                          
626224     03 FILLER                   PIC X          VALUE ';'.                
626324     03 WS-REDIRLEV              PIC 9.9(2).                              
626424     03 FILLER                   PIC X          VALUE ';'.                
626524     03 WS-N-IDLEVNR-PR          PIC X(5).                                
626624     03 FILLER                   PIC X          VALUE ';'.                
626724     03 WS-N-KDPRURSP            PIC X.                                   
626824     03 FILLER                   PIC X          VALUE ';'.                
626924     03 WS-N-TIPRLIST            PIC Z(6)9.                               
627024     03 FILLER                   PIC X          VALUE ';'.                
627124     03 WS-N-PRARTBES-PR         PIC Z(6)9.9(2).                          
627224     03 FILLER                   PIC X          VALUE ';'.                
627324     03 WS-N-PRARTBEL-PR         PIC Z(7)9.9(5).                          
627424     03 FILLER                   PIC X          VALUE ';'.                
627524     03 WS-N-KDSTATUS-PR         PIC 9.                                   
627624     03 FILLER                   PIC X          VALUE ';'.                
627724     03 WS-N-SUINLEV-PR          PIC Z(2)9.                               
627824     03 FILLER                   PIC X          VALUE ';'.                
627924     03 WS-N-KDVALISO            PIC X(3).                                
628024     03 FILLER                   PIC X          VALUE ';'.                
628124     03 WS-N-PRARTBES            PIC Z(6)9.9(2).                          
628224     03 FILLER                   PIC X          VALUE ';'.                
628324     03 WS-N-PRARTSJK            PIC Z(6)9.9(2).                          
628424     03 FILLER                   PIC X          VALUE ';'.                
628524     03 WS-N-RETULF              PIC Z(2)9.9(4).                          
628624     03 FILLER                   PIC X          VALUE ';'.                
628724     03 WS-N-PRLFKST             PIC Z(2)9.9(2).                          
628824     03 FILLER                   PIC X          VALUE ';'.                
628924     03 WS-N-KDCMD               PIC X.                                   
629024     03 FILLER                   PIC X          VALUE ';'.                
629124     03 WS-N-PRARTSTD            PIC Z(6)9.9(2).                          
629224     03 FILLER                   PIC X          VALUE ';'.                
629324     03 WS-N-PRINK               PIC Z(6)9.9(2).                          
629424     03 FILLER                   PIC X          VALUE ';'.                
629524     03 WS-FLPRIBES              PIC X.                                   
629624     03 FILLER                   PIC X          VALUE ';'.                
629724     03 WS-FLPRIGO               PIC X.                                   
629824     03 FILLER                   PIC X          VALUE ';'.                
629924     03 WS-KDPRIBEH              PIC X.                                   
630024     03 FILLER                   PIC X          VALUE ';'.                
630124     03 WS-FLPRFIL               PIC X.                                   
630224     03 FILLER                   PIC X          VALUE ';'.                
630324     03 WS-INK-ANDEL             PIC -(7)9.9(2).                          
630424     03 FILLER                   PIC X          VALUE ';'.                
630524     03 WS-STD-ANDEL             PIC -(7)9.9(2).                          
630624     03 FILLER                   PIC X          VALUE ';'.                
630524     03 WS-WEEK                  PIC X(2).                                
630624     03 FILLER                   PIC X          VALUE ';'.                
630724     03 WS-IDUSER                PIC X(20).                               
630824     03 FILLER                   PIC X          VALUE ';'.                
630924     03 WS-COMMENT               PIC X(21).                               
631121     SKIP2                                                                
632000     EJECT                                                                
633000 01 HEADER-AREA.                                                          
621624     03 HD-REPORT-TYPE           PIC X(11)      VALUE                     
621624                                 'REPORT TYPE'.                           
621724     03 FILLER                   PIC X          VALUE ';'.                
621624     03 HD-REPORT-DESC           PIC X(18)      VALUE                     
621624                                 'REPORT DESCRIPTION'.                    
621724     03 FILLER                   PIC X          VALUE ';'.                
623224     03 HD-IDARTNR               PIC X(7)       VALUE                     
623324                                 'PART NO'.                               
623424     03 FILLER                   PIC X          VALUE ';'.                
623524     03 HD-DAREGDAT              PIC X(17)      VALUE                     
623624                                 'REGISTRATION DATE'.                     
623724     03 FILLER                   PIC X          VALUE ';'.                
623824     03 HD-O-IDLEVNR-PR          PIC X(15)      VALUE                     
623924                                 'OLD SUPPLIER NO'.                       
624024     03 FILLER                   PIC X          VALUE ';'.                
624124     03 HD-O-KDPRURSP            PIC X(22)      VALUE                     
624224                                 'OLD ORDER PRICE ORIGIN'.                
624324     03 FILLER                   PIC X          VALUE ';'.                
624424     03 HD-O-TIPRLIST            PIC X(19)      VALUE                     
624524                                 'OLD PRICE LIST DATE'.                   
624624     03 FILLER                   PIC X          VALUE ';'.                
624724     03 HD-O-PRARTBES-PR         PIC X(23)      VALUE                     
624824                                 'OLD SWEDISH ORDER PRICE'.               
624924     03 FILLER                   PIC X          VALUE ';'.                
625024     03 HD-O-PRARTBEL-PR         PIC X(25)      VALUE                     
625124                                 'OLD SUPPLIERS ORDER PRICE'.             
625224     03 FILLER                   PIC X          VALUE ';'.                
625324     03 HD-O-KDSTATUS-PR         PIC X(10)      VALUE                     
625424                                 'OLD STATUS'.                            
625524     03 FILLER                   PIC X          VALUE ';'.                
623224     03 HD-O-SUINLEV-PR          PIC X(29)      VALUE                     
623324                                 'OLD NO OF INCOMING DELIVERIES'.         
623424     03 FILLER                   PIC X          VALUE ';'.                
623524     03 HD-O-KDVALISO            PIC X(17)      VALUE                     
623624                                 'OLD CURRENCY CODE'.                     
623724     03 FILLER                   PIC X          VALUE ';'.                
623824     03 HD-O-PRARTBES            PIC X(15)      VALUE                     
623924                                 'OLD ORDER PRICE'.                       
624024     03 FILLER                   PIC X          VALUE ';'.                
624124     03 HD-O-PRARTSJK            PIC X(17)      VALUE                     
624224                                 'OLD COST OF SALES'.                     
624324     03 FILLER                   PIC X          VALUE ';'.                
624424     03 HD-O-RETULF              PIC X(16)      VALUE                     
624524                                 'OLD RATE OF DUTY'.                      
624624     03 FILLER                   PIC X          VALUE ';'.                
624724     03 HD-O-PRLFKST             PIC X(29)      VALUE                     
624824                                 'OLD PACKING AND HANDLING COST'.         
624924     03 FILLER                   PIC X          VALUE ';'.                
625024     03 HD-O-KDCMD               PIC X(16)      VALUE                     
625124                                 'OLD COMMAND CODE'.                      
625224     03 FILLER                   PIC X          VALUE ';'.                
625324     03 HD-O-PRARTSTD            PIC X(18)      VALUE                     
625424                                 'OLD STANDARD PRICE'.                    
625524     03 FILLER                   PIC X          VALUE ';'.                
625624     03 HD-O-PRINK               PIC X(18)      VALUE                     
625724                                 'OLD PURCHASE PRICE'.                    
625824     03 FILLER                   PIC X          VALUE ';'.                
623224     03 HD-O-PRDIRLON            PIC X(16)      VALUE                     
623324                                 'OLD DIRECT WAGES'.                      
623424     03 FILLER                   PIC X          VALUE ';'.                
623524     03 HD-O-PRDMTRL             PIC X(19)      VALUE                     
623624                                 'OLD SURCHARGE PRICE'.                   
623724     03 FILLER                   PIC X          VALUE ';'.                
623824     03 HD-O-PROVRPAL            PIC X(22)      VALUE                     
623924                                 'OLD OVERHEAD SURCHARGE'.                
624024     03 FILLER                   PIC X          VALUE ';'.                
624124     03 HD-REDIRLEV              PIC X(21)      VALUE                     
624224                                 'DIRECT DELIVERY SHARE'.                 
624324     03 FILLER                   PIC X          VALUE ';'.                
624424     03 HD-N-IDLEVNR-PR          PIC X(15)      VALUE                     
624524                                 'NEW SUPPLIER NO'.                       
624624     03 FILLER                   PIC X          VALUE ';'.                
624724     03 HD-N-KDPRURSP            PIC X(22)      VALUE                     
624824                                 'NEW ORDER PRICE ORIGIN'.                
624924     03 FILLER                   PIC X          VALUE ';'.                
625024     03 HD-N-TIPRLIST            PIC X(19)      VALUE                     
625124                                 'NEW PRICE LIST DATE'.                   
625224     03 FILLER                   PIC X          VALUE ';'.                
625324     03 HD-N-PRARTBES-PR         PIC X(23)      VALUE                     
625424                                 'NEW SWEDISH ORDER PRICE'.               
625524     03 FILLER                   PIC X          VALUE ';'.                
625624     03 HD-N-PRARTBEL-PR         PIC X(25)      VALUE                     
625724                                 'NEW SUPPLIERS ORDER PRICE'.             
627424     03 FILLER                   PIC X          VALUE ';'.                
627524     03 HD-N-KDSTATUS-PR         PIC X(10)      VALUE                     
623224                                 'NEW STATUS'.                            
623324     03 FILLER                   PIC X          VALUE ';'.                
623424     03 HD-N-SUINLEV-PR          PIC X(29)      VALUE                     
623524                                 'NEW NO OF INCOMING DELIVERIES'.         
623624     03 FILLER                   PIC X          VALUE ';'.                
623724     03 HD-N-KDVALISO            PIC X(17)      VALUE                     
623824                                 'NEW CURRENCY CODE'.                     
623924     03 FILLER                   PIC X          VALUE ';'.                
624024     03 HD-N-PRARTBES            PIC X(15)      VALUE                     
624124                                 'NEW ORDER PRICE'.                       
624224     03 FILLER                   PIC X          VALUE ';'.                
624324     03 HD-N-PRARTSJK            PIC X(17)      VALUE                     
624424                                 'NEW COST OF SALES'.                     
624524     03 FILLER                   PIC X          VALUE ';'.                
624624     03 HD-N-RETULF              PIC X(16)      VALUE                     
624724                                 'NEW RATE OF DUTY'.                      
624824     03 FILLER                   PIC X          VALUE ';'.                
624924     03 HD-N-PRLFKST             PIC X(29)      VALUE                     
625024                                 'NEW PACKING AND HANDLING COST'.         
625124     03 FILLER                   PIC X          VALUE ';'.                
625224     03 HD-N-KDCMD               PIC X(16)      VALUE                     
625324                                 'NEW COMMAND CODE'.                      
625424     03 FILLER                   PIC X          VALUE ';'.                
625524     03 HD-N-PRARTSTD            PIC X(18)      VALUE                     
625624                                 'NEW STANDARD PRICE'.                    
625724     03 FILLER                   PIC X          VALUE ';'.                
629324     03 HD-N-PRINK               PIC X(18)      VALUE                     
623224                                 'NEW PURCHASE PRICE'.                    
623324     03 FILLER                   PIC X          VALUE ';'.                
623424     03 HD-FLPRIBES              PIC X(27)      VALUE                     
623524                                 'SUPPLIERS PRICE UPDATE FLAG'.           
623624     03 FILLER                   PIC X          VALUE ';'.                
623724     03 HD-FLPRIGO               PIC X(23)      VALUE                     
623824                                 'BIG PRICE INCREASE FLAG'.               
623924     03 FILLER                   PIC X          VALUE ';'.                
624024     03 HD-KDPRIBEH              PIC X(20)      VALUE                     
624124                                 'PRICE TREATMENT CODE'.                  
624224     03 FILLER                   PIC X          VALUE ';'.                
624324     03 HD-FLPRFIL               PIC X(10)      VALUE                     
624424                                 'PRICE FLAG'.                            
624524     03 FILLER                   PIC X          VALUE ';'.                
624624     03 HD-INK-ANDEL             PIC X(30)      VALUE                     
624724                                 'PURCHASE PRICE PERCENTAGE DIFF'.        
624824     03 FILLER                   PIC X          VALUE ';'.                
624924     03 HD-STD-ANDEL             PIC X(30)      VALUE                     
625024                                 'STANDARD PRICE PERCENTAGE DIFF'.        
625124     03 FILLER                   PIC X          VALUE ';'.                
624924     03 HD-WEEK                  PIC X(16)      VALUE                     
625024                                 'WEEK OF THE YEAR'.                      
625124     03 FILLER                   PIC X          VALUE ';'.                
625224     03 HD-IDUSER                PIC X(07)      VALUE                     
625324                                 'USER ID'.                               
625424     03 FILLER                   PIC X          VALUE ';'.                
625524     03 HD-COMMENT               PIC X(07)      VALUE                     
625624                                 'COMMENT'.                               
625724 PROCEDURE DIVISION.                                                      
625824 MAIN SECTION.                                                            
670000     SKIP2                                                                
680000                                                                          
690000     PERFORM A-INIT                                                       
701000     PERFORM S01-READ-W55324                                              
710001     PERFORM UNTIL END-OF-W55324                                          
711005       PERFORM B-EVALUATE-REPORT-TYPE                                     
760000                                                                          
770000                                                                          
781000       PERFORM S01-READ-W55324                                            
790000     END-PERFORM                                                          
800000                                                                          
810000                                                                          
820000     PERFORM Z-FINIT                                                      
830000                                                                          
840000     MOVE ZERO TO RETURN-CODE                                             
850000     GOBACK                                                               
860000     .                                                                    
870000     EJECT                                                                
880000 A-INIT SECTION.                                                          
890100                                                                          
891000     OPEN INPUT  W55324                                                   
900100                                                                          
901000     OPEN OUTPUT W55333                                                   
902022                                                                          
903023     INITIALIZE TEMP-AREA                                                 
953216                WS-TP-COMMENT                                             
953309                WS-TP-REPORT-TYPE                                         
953309                WS-TP-REPORT-DESC                                         
953407                WS-TP-INK-ANDEL                                           
953507                WS-TP-STD-ANDEL                                           
953607                WS-TP-WEEK                                                
953707                                                                          
920000     ACCEPT TODAYS-DATE  FROM DATE                                        
931000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
932018     CALL DATKORT USING IDPGM DATUMKORT-ID DATUMKORT                      
934018     MOVE 20           TO WS-DAGENS-DATUM(1:2)                            
935018     MOVE D-AAR        TO WS-DAGENS-DATUM(3:2)                            
936018     MOVE D-MAANAD     TO WS-DAGENS-DATUM(5:2)                            
937018     MOVE D-DAG        TO WS-DAGENS-DATUM(7:2)                            
937018     MOVE D-VECKA      TO WS-TEMP-WEEK                                    
953216     PERFORM S12-WRITE-HEADER                                             
953309     .                                                                    
953407     EJECT                                                                
953507 B-EVALUATE-REPORT-TYPE SECTION.                                          
953607                                                                          
953707     IF IN-PRI-FLKLAR = 'J'                                               
953216        INITIALIZE TEMP-AREA                                              
953309                   WS-TP-COMMENT                                          
953407                   WS-TP-REPORT-TYPE                                      
953407                   WS-TP-REPORT-DESC                                      
953507                   WS-TP-INK-ANDEL                                        
953607                   WS-TP-STD-ANDEL                                        
952216        MOVE 'W55327-001'      TO WS-TP-REPORT-TYPE                       
953707        MOVE 'R25 CHANGE OF PRICE AND COST'                               
952216                               TO WS-TP-REPORT-DESC                       
952308        PERFORM BA-EVALUATE-IDUSER                                        
952409        PERFORM BB-POPULATE-REPORT1-DATA                                  
952505     END-IF                                                               
952607     IF IN-PRI-FLPRIGO = 'J'                                              
953216        INITIALIZE TEMP-AREA                                              
953309                   WS-TP-COMMENT                                          
953407                   WS-TP-REPORT-TYPE                                      
953407                   WS-TP-REPORT-DESC                                      
953507                   WS-TP-INK-ANDEL                                        
953607                   WS-TP-STD-ANDEL                                        
953607                   WS-TP-WEEK                                             
952716        MOVE 'W55328-001'      TO WS-TP-REPORT-TYPE                       
953707        MOVE 'R25 PARTS WITH BIG PRICE CHANGE'                            
952216                               TO WS-TP-REPORT-DESC                       
952808        PERFORM BA-EVALUATE-IDUSER                                        
952909        PERFORM BC-POPULATE-REPORT2-DATA                                  
953007     END-IF                                                               
953107     IF IN-PRI-KDPRIBEH = 'B'                                             
953216        INITIALIZE TEMP-AREA                                              
953309                   WS-TP-COMMENT                                          
953407                   WS-TP-REPORT-TYPE                                      
953407                   WS-TP-REPORT-DESC                                      
953507                   WS-TP-INK-ANDEL                                        
953607                   WS-TP-STD-ANDEL                                        
953607                   WS-TP-WEEK                                             
953216        MOVE 'W55329-001'      TO WS-TP-REPORT-TYPE                       
953707        MOVE 'R25 REMOVED PRICES'                                         
952216                               TO WS-TP-REPORT-DESC                       
953309        PERFORM BD-POPULATE-REPORT3-DATA                                  
953407     END-IF                                                               
953507     IF IN-PRI-KDPRIBEH NOT = 'A'                                         
953607     AND IN-PRI-N-PRARTSTD NOT = +0                                       
953707     AND IN-PRI-N-PRARTSTD NOT = IN-PRI-O-PRARTSTD                        
953216        INITIALIZE TEMP-AREA                                              
953309                   WS-TP-COMMENT                                          
953407                   WS-TP-REPORT-TYPE                                      
953407                   WS-TP-REPORT-DESC                                      
953507                   WS-TP-INK-ANDEL                                        
953607                   WS-TP-STD-ANDEL                                        
953607                   WS-TP-WEEK                                             
953816        MOVE 'W55330-001'      TO WS-TP-REPORT-TYPE                       
953707        MOVE 'CHANGED STANDARD PRICE'                                     
952216                               TO WS-TP-REPORT-DESC                       
953908        PERFORM BA-EVALUATE-IDUSER                                        
954009        PERFORM BE-POPULATE-REPORT4-DATA                                  
954107     END-IF                                                               
954207     IF IN-PRI-KDPRIBEH = 'A'                                             
953816        INITIALIZE TEMP-AREA                                              
953908                   WS-TP-COMMENT                                          
953816                   WS-TP-REPORT-TYPE                                      
953816                   WS-TP-REPORT-DESC                                      
953908                   WS-TP-INK-ANDEL                                        
954009                   WS-TP-STD-ANDEL                                        
954009                   WS-TP-WEEK                                             
954207        MOVE 'W55332-001'      TO WS-TP-REPORT-TYPE                       
953707        MOVE 'PRICES FOR NEW PARTS'                                       
952216                               TO WS-TP-REPORT-DESC                       
954409        PERFORM BF-POPULATE-REPORT5-DATA                                  
954507     END-IF                                                               
954607     .                                                                    
954707     EJECT                                                                
955007 BA-EVALUATE-IDUSER SECTION.                                              
955115     EVALUATE IN-PRI-IDUSER                                               
955207        WHEN 'SATS    '                                                   
955316           MOVE 'SATSKÖRNING W55316  '      TO WS-TP-USER-ID              
955416           MOVE 'M'                         TO TP-PRI-N-KDPRURSP          
955507        WHEN 'INKOP   '                                                   
955616           MOVE 'PV INKÖP W55312     '      TO WS-TP-USER-ID              
955716           MOVE 'M'                         TO TP-PRI-N-KDPRURSP          
955807        WHEN 'NEDCAR  '                                                   
955916           MOVE 'NEDCAR INKÖP W55312 '      TO WS-TP-USER-ID              
956016           MOVE 'M'                         TO TP-PRI-N-KDPRURSP          
956107        WHEN 'LANDROV '                                                   
956216           MOVE 'LAND ROVER W16110   '      TO WS-TP-USER-ID              
956316           MOVE 'M'                         TO TP-PRI-N-KDPRURSP          
956407        WHEN 'RENAULT '                                                   
956516           MOVE 'RENAULT PRISFIL     '      TO WS-TP-USER-ID              
956616           MOVE 'M'                         TO TP-PRI-N-KDPRURSP          
956707        WHEN 'GSDB    '                                                   
956816           MOVE 'GSDB-KONVERTERING   '      TO WS-TP-USER-ID              
956916           MOVE 'M'                         TO TP-PRI-N-KDPRURSP          
957007        WHEN OTHER                                                        
957116           MOVE IN-PRI-IDUSER               TO WS-TP-USER-ID              
956916           MOVE IN-PRI-N-KDPRURSP           TO TP-PRI-N-KDPRURSP          
957207     END-EVALUATE                                                         
958007     .                                                                    
959007     EJECT                                                                
959109 BB-POPULATE-REPORT1-DATA  SECTION.                                       
959216     MOVE IN-PRI-O-IDLEVNR-PR      TO TP-PRI-O-IDLEVNR-PR                 
959316     MOVE IN-PRI-IDARTNR           TO TP-PRI-IDARTNR                      
959416     MOVE IN-PRI-DAREGDAT          TO TP-PRI-DAREGDAT                     
959616     MOVE IN-PRI-N-TIPRLIST        TO TP-PRI-N-TIPRLIST                   
959716     MOVE IN-PRI-N-IDLEVNR-PR      TO TP-PRI-N-IDLEVNR-PR                 
959816     MOVE IN-PRI-N-PRARTBES-PR     TO TP-PRI-N-PRARTBES-PR                
959916     MOVE IN-PRI-N-PRARTBEL-PR     TO TP-PRI-N-PRARTBEL-PR                
960016     MOVE IN-PRI-N-KDSTATUS-PR     TO TP-PRI-N-KDSTATUS-PR                
960116     MOVE IN-PRI-N-SUINLEV-PR      TO TP-PRI-N-SUINLEV-PR                 
960216     MOVE IN-PRI-N-KDVALISO        TO TP-PRI-N-KDVALISO                   
960313     IF IN-PRI-O-TIPRLIST NOT = +0                                        
960416        MOVE IN-PRI-O-KDPRURSP     TO TP-PRI-O-KDPRURSP                   
960516        MOVE IN-PRI-O-TIPRLIST     TO TP-PRI-O-TIPRLIST                   
960616        MOVE IN-PRI-O-PRARTBES-PR  TO TP-PRI-O-PRARTBES-PR                
960716        MOVE IN-PRI-O-PRARTBEL-PR  TO TP-PRI-O-PRARTBEL-PR                
960816        MOVE IN-PRI-O-KDSTATUS-PR  TO TP-PRI-O-KDSTATUS-PR                
960916        MOVE IN-PRI-O-SUINLEV-PR   TO TP-PRI-O-SUINLEV-PR                 
961016        MOVE IN-PRI-O-KDVALISO     TO TP-PRI-O-KDVALISO                   
961116        MOVE IN-PRI-O-PRARTBES     TO TP-PRI-O-PRARTBES                   
961216        MOVE IN-PRI-O-PRARTSJK     TO TP-PRI-O-PRARTSJK                   
961316        MOVE IN-PRI-O-RETULF       TO TP-PRI-O-RETULF                     
961416        MOVE IN-PRI-O-PRLFKST      TO TP-PRI-O-PRLFKST                    
961516        MOVE IN-PRI-O-KDCMD        TO TP-PRI-O-KDCMD                      
961716        MOVE IN-PRI-REDIRLEV       TO TP-PRI-REDIRLEV                     
961816        MOVE IN-PRI-N-PRARTBES     TO TP-PRI-N-PRARTBES                   
961916        MOVE IN-PRI-N-PRARTSJK     TO TP-PRI-N-PRARTSJK                   
962016        MOVE IN-PRI-N-RETULF       TO TP-PRI-N-RETULF                     
962116        MOVE IN-PRI-N-PRLFKST      TO TP-PRI-N-PRLFKST                    
962216        MOVE IN-PRI-N-KDCMD        TO TP-PRI-N-KDCMD                      
962516        MOVE IN-PRI-FLPRIBES       TO TP-PRI-FLPRIBES                     
962616        MOVE IN-PRI-FLPRIGO        TO TP-PRI-FLPRIGO                      
962616        PERFORM BZ-MOVE-TEMP-TO-OP-AREA                                   
962714        PERFORM S11-WRITE-W55333                                          
962814     ELSE                                                                 
962915        MOVE 'NY PRELIMINÄR PRISRAD'  TO WS-TP-COMMENT                    
962616        PERFORM BZ-MOVE-TEMP-TO-OP-AREA                                   
963014        PERFORM S11-WRITE-W55333                                          
963114     END-IF                                                               
963214     .                                                                    
963314     EJECT                                                                
963414 BC-POPULATE-REPORT2-DATA  SECTION.                                       
963616     MOVE IN-PRI-IDARTNR           TO TP-PRI-IDARTNR                      
963716     MOVE IN-PRI-DAREGDAT          TO TP-PRI-DAREGDAT                     
963916     MOVE IN-PRI-O-KDPRURSP        TO TP-PRI-O-KDPRURSP                   
964016     MOVE IN-PRI-O-TIPRLIST        TO TP-PRI-O-TIPRLIST                   
964116     MOVE IN-PRI-O-IDLEVNR-PR      TO TP-PRI-O-IDLEVNR-PR                 
964216     MOVE IN-PRI-O-PRARTBES-PR     TO TP-PRI-O-PRARTBES-PR                
964316     MOVE IN-PRI-O-PRARTBEL-PR     TO TP-PRI-O-PRARTBEL-PR                
964416     MOVE IN-PRI-O-KDSTATUS-PR     TO TP-PRI-O-KDSTATUS-PR                
964516     MOVE IN-PRI-O-SUINLEV-PR      TO TP-PRI-O-SUINLEV-PR                 
964616     MOVE IN-PRI-O-KDVALISO        TO TP-PRI-O-KDVALISO                   
964716     MOVE IN-PRI-O-PRARTBES        TO TP-PRI-O-PRARTBES                   
964816     MOVE IN-PRI-O-PRARTSJK        TO TP-PRI-O-PRARTSJK                   
964916     MOVE IN-PRI-O-RETULF          TO TP-PRI-O-RETULF                     
965016     MOVE IN-PRI-O-PRLFKST         TO TP-PRI-O-PRLFKST                    
965116     MOVE IN-PRI-O-KDCMD           TO TP-PRI-O-KDCMD                      
965316     MOVE IN-PRI-N-TIPRLIST        TO TP-PRI-N-TIPRLIST                   
965416     MOVE IN-PRI-N-IDLEVNR-PR      TO TP-PRI-N-IDLEVNR-PR                 
965516     MOVE IN-PRI-N-PRARTBES-PR     TO TP-PRI-N-PRARTBES-PR                
965616     MOVE IN-PRI-N-PRARTBEL-PR     TO TP-PRI-N-PRARTBEL-PR                
965716     MOVE IN-PRI-N-KDSTATUS-PR     TO TP-PRI-N-KDSTATUS-PR                
965816     MOVE IN-PRI-N-SUINLEV-PR      TO TP-PRI-N-SUINLEV-PR                 
965916     MOVE IN-PRI-N-KDVALISO        TO TP-PRI-N-KDVALISO                   
966016     MOVE IN-PRI-N-PRARTBES        TO TP-PRI-N-PRARTBES                   
966116     MOVE IN-PRI-N-PRARTSJK        TO TP-PRI-N-PRARTSJK                   
966216     MOVE IN-PRI-N-RETULF          TO TP-PRI-N-RETULF                     
966316     MOVE IN-PRI-N-PRLFKST         TO TP-PRI-N-PRLFKST                    
966416     MOVE IN-PRI-N-KDCMD           TO TP-PRI-N-KDCMD                      
966516     MOVE IN-PRI-REDIRLEV          TO TP-PRI-REDIRLEV                     
966616     MOVE IN-PRI-FLPRIBES          TO TP-PRI-FLPRIBES                     
966716     MOVE IN-PRI-FLPRIGO           TO TP-PRI-FLPRIGO                      
966716     PERFORM BZ-MOVE-TEMP-TO-OP-AREA                                      
966815     PERFORM S11-WRITE-W55333                                             
966914     .                                                                    
967014     EJECT                                                                
967114 BD-POPULATE-REPORT3-DATA  SECTION.                                       
967316     MOVE IN-PRI-IDARTNR           TO TP-PRI-IDARTNR                      
967416     MOVE IN-PRI-DAREGDAT          TO TP-PRI-DAREGDAT                     
967516     MOVE IN-PRI-O-KDPRURSP        TO TP-PRI-O-KDPRURSP                   
967616     MOVE IN-PRI-O-TIPRLIST        TO TP-PRI-O-TIPRLIST                   
967716     MOVE IN-PRI-O-IDLEVNR-PR      TO TP-PRI-O-IDLEVNR-PR                 
967816     MOVE IN-PRI-O-PRARTBES-PR     TO TP-PRI-O-PRARTBES-PR                
967916     MOVE IN-PRI-O-PRARTBEL-PR     TO TP-PRI-O-PRARTBEL-PR                
968016     MOVE IN-PRI-O-KDSTATUS-PR     TO TP-PRI-O-KDSTATUS-PR                
968116     MOVE IN-PRI-O-SUINLEV-PR      TO TP-PRI-O-SUINLEV-PR                 
968216     MOVE IN-PRI-O-KDVALISO        TO TP-PRI-O-KDVALISO                   
968316     MOVE IN-PRI-O-KDCMD           TO TP-PRI-O-KDCMD                      
968416     MOVE IN-PRI-IDUSER            TO WS-TP-USER-ID                       
966716     PERFORM BZ-MOVE-TEMP-TO-OP-AREA                                      
968515     PERFORM S11-WRITE-W55333                                             
968614     .                                                                    
968714     EJECT                                                                
968814 BE-POPULATE-REPORT4-DATA  SECTION.                                       
968917     MOVE IN-PRI-IDARTNR           TO TP-PRI-IDARTNR                      
969017     MOVE IN-PRI-DAREGDAT          TO TP-PRI-DAREGDAT                     
969117     MOVE IN-PRI-N-PRARTBEL-PR     TO TP-PRI-N-PRARTBEL-PR                
969217     MOVE IN-PRI-KDPRIBEH          TO TP-PRI-KDPRIBEH                     
969317     MOVE IN-PRI-N-KDVALISO        TO TP-PRI-N-KDVALISO                   
969417     MOVE IN-PRI-FLPRFIL           TO TP-PRI-FLPRFIL                      
969517     MOVE IN-PRI-O-PRINK           TO TP-PRI-O-PRINK                      
969617     MOVE IN-PRI-N-PRINK           TO TP-PRI-N-PRINK                      
969717     IF IN-PRI-O-PRINK NOT = +0                                           
969817     AND IN-PRI-N-PRINK NOT = +0                                          
969917       COMPUTE WS-TP-INK-ANDEL =                                          
970017               ((IN-PRI-N-PRINK / IN-PRI-O-PRINK ) - 1) * 100             
970017     ELSE                                                                 
970117       IF IN-PRI-O-PRINK = +0                                             
970217       AND IN-PRI-N-PRINK = +0                                            
970317          MOVE +0                  TO WS-TP-INK-ANDEL                     
970417       ELSE                                                               
970517          MOVE +999.99             TO WS-TP-INK-ANDEL                     
970617       END-IF                                                             
970817     END-IF                                                               
970917     MOVE IN-PRI-O-PRARTSTD        TO TP-PRI-O-PRARTSTD                   
971017     MOVE IN-PRI-N-PRARTSTD        TO TP-PRI-N-PRARTSTD                   
971117     IF IN-PRI-O-PRARTSTD NOT = +0                                        
971217     AND IN-PRI-N-PRARTSTD NOT = +0                                       
971317       COMPUTE WS-TP-STD-ANDEL =                                          
971417            ((IN-PRI-N-PRARTSTD / IN-PRI-O-PRARTSTD ) - 1) * 100          
971417     ELSE                                                                 
971517       IF IN-PRI-O-PRARTSTD = +0                                          
971617       AND IN-PRI-N-PRARTSTD = +0                                         
971717          MOVE +0                  TO WS-TP-STD-ANDEL                     
971817       ELSE                                                               
971917          MOVE +999.99             TO WS-TP-STD-ANDEL                     
972017       END-IF                                                             
972117     END-IF                                                               
972317     MOVE IN-PRI-O-PRDIRLON        TO TP-PRI-O-PRDIRLON                   
972417     MOVE IN-PRI-O-PRDMTRL         TO TP-PRI-O-PRDMTRL                    
972517     MOVE IN-PRI-O-PROVRPAL        TO TP-PRI-O-PROVRPAL                   
972517     MOVE SPACES                   TO TP-PRI-N-KDPRURSP                   
966716     PERFORM BZ-MOVE-TEMP-TO-OP-AREA                                      
968515     PERFORM S11-WRITE-W55333                                             
972617     .                                                                    
972717     EJECT                                                                
972817 BF-POPULATE-REPORT5-DATA  SECTION.                                       
972918                                                                          
973018     MOVE IN-PRI-IDARTNR           TO TP-PRI-IDARTNR                      
973118     MOVE IN-PRI-DAREGDAT          TO TP-PRI-DAREGDAT                     
973218     MOVE IN-PRI-N-IDLEVNR-PR      TO TP-PRI-N-IDLEVNR-PR                 
973318     MOVE IN-PRI-N-PRINK           TO TP-PRI-N-PRINK                      
973418     MOVE IN-PRI-N-PRARTSTD        TO TP-PRI-N-PRARTSTD                   
973518     MOVE IN-PRI-N-PRARTBES        TO TP-PRI-N-PRARTBES                   
973618     MOVE IN-PRI-N-PRARTSJK        TO TP-PRI-N-PRARTSJK                   
973718     MOVE IN-PRI-IDUSER            TO WS-TP-USER-ID                       
973718     MOVE WS-TEMP-WEEK             TO WS-TP-WEEK                          
966716     PERFORM BZ-MOVE-TEMP-TO-OP-AREA                                      
968515     PERFORM S11-WRITE-W55333                                             
973818     .                                                                    
973917     EJECT                                                                
974017 BZ-MOVE-TEMP-TO-OP-AREA SECTION.                                         
621624     MOVE WS-TP-REPORT-TYPE        TO WS-REPORT-TYPE                      
621624     MOVE WS-TP-REPORT-DESC        TO WS-REPORT-DESC                      
621824     MOVE TP-PRI-IDARTNR           TO WS-IDARTNR                          
622024     MOVE TP-PRI-DAREGDAT          TO WS-DAREGDAT                         
622224     MOVE TP-PRI-O-IDLEVNR-PR      TO WS-O-IDLEVNR-PR                     
622924     MOVE TP-PRI-O-KDPRURSP        TO WS-O-KDPRURSP                       
623124     MOVE TP-PRI-O-TIPRLIST        TO WS-O-TIPRLIST                       
623324     MOVE TP-PRI-O-PRARTBES-PR     TO WS-O-PRARTBES-PR                    
623524     MOVE TP-PRI-O-PRARTBEL-PR     TO WS-O-PRARTBEL-PR                    
623724     MOVE TP-PRI-O-KDSTATUS-PR     TO WS-O-KDSTATUS-PR                    
623924     MOVE TP-PRI-O-SUINLEV-PR      TO WS-O-SUINLEV-PR                     
624124     MOVE TP-PRI-O-KDVALISO        TO WS-O-KDVALISO                       
624324     MOVE TP-PRI-O-PRARTBES        TO WS-O-PRARTBES                       
624524     MOVE TP-PRI-O-PRARTSJK        TO WS-O-PRARTSJK                       
624724     MOVE TP-PRI-O-RETULF          TO WS-O-RETULF                         
624924     MOVE TP-PRI-O-PRLFKST         TO WS-O-PRLFKST                        
625124     MOVE TP-PRI-O-KDCMD           TO WS-O-KDCMD                          
625324     MOVE TP-PRI-O-PRARTSTD        TO WS-O-PRARTSTD                       
625524     MOVE TP-PRI-O-PRINK           TO WS-O-PRINK                          
625724     MOVE TP-PRI-O-PRDIRLON        TO WS-O-PRDIRLON                       
625924     MOVE TP-PRI-O-PRDMTRL         TO WS-O-PRDMTRL                        
626124     MOVE TP-PRI-O-PROVRPAL        TO WS-O-PROVRPAL                       
626324     MOVE TP-PRI-REDIRLEV          TO WS-REDIRLEV                         
626524     MOVE TP-PRI-N-IDLEVNR-PR      TO WS-N-IDLEVNR-PR                     
626724     MOVE TP-PRI-N-KDPRURSP        TO WS-N-KDPRURSP                       
626924     MOVE TP-PRI-N-TIPRLIST        TO WS-N-TIPRLIST                       
627124     MOVE TP-PRI-N-PRARTBES-PR     TO WS-N-PRARTBES-PR                    
627324     MOVE TP-PRI-N-PRARTBEL-PR     TO WS-N-PRARTBEL-PR                    
627524     MOVE TP-PRI-N-KDSTATUS-PR     TO WS-N-KDSTATUS-PR                    
627724     MOVE TP-PRI-N-SUINLEV-PR      TO WS-N-SUINLEV-PR                     
627924     MOVE TP-PRI-N-KDVALISO        TO WS-N-KDVALISO                       
628124     MOVE TP-PRI-N-PRARTBES        TO WS-N-PRARTBES                       
628324     MOVE TP-PRI-N-PRARTSJK        TO WS-N-PRARTSJK                       
628524     MOVE TP-PRI-N-RETULF          TO WS-N-RETULF                         
628724     MOVE TP-PRI-N-PRLFKST         TO WS-N-PRLFKST                        
628924     MOVE TP-PRI-N-KDCMD           TO WS-N-KDCMD                          
629124     MOVE TP-PRI-N-PRARTSTD        TO WS-N-PRARTSTD                       
629324     MOVE TP-PRI-N-PRINK           TO WS-N-PRINK                          
629524     MOVE TP-PRI-FLPRIBES          TO WS-FLPRIBES                         
629724     MOVE TP-PRI-FLPRIGO           TO WS-FLPRIGO                          
629924     MOVE TP-PRI-KDPRIBEH          TO WS-KDPRIBEH                         
630124     MOVE TP-PRI-FLPRFIL           TO WS-FLPRFIL                          
630324     MOVE WS-TP-INK-ANDEL          TO WS-INK-ANDEL                        
630524     MOVE WS-TP-STD-ANDEL          TO WS-STD-ANDEL                        
630524     MOVE WS-TP-WEEK               TO WS-WEEK                             
630724     MOVE WS-TP-USER-ID            TO WS-IDUSER                           
630924     MOVE WS-TP-COMMENT            TO WS-COMMENT                          
630924     .                                                                    
974017 Z-FINIT SECTION.                                                         
974117     CLOSE W55324                                                         
975017           W55333                                                         
980100     SKIP2                                                                
980200     MOVE 'S' TO POSTSUM-OPKOD                                            
981000     CALL POSTSUM USING POSTSUM-PARM                                      
990000     .                                                                    
000100     EJECT                                                                
000200 S01-READ-W55324  SECTION.                                                
000300     READ W55324 INTO IN-AREA                                             
000400     AT END                                                               
000500        MOVE HIGH-VALUE TO IN-AREA                                        
000600        SET END-OF-W55324 TO TRUE                                         
000700                                                                          
000800     NOT AT END                                                           
000900        MOVE 'W55324' TO POSTSUM-FDNAMN                                   
001000        MOVE 'W55333D1' TO POSTSUM-DDNAMN2                                
001203        MOVE SPACES    TO POSTSUM-TRANSTYP                                
001300        CALL POSTSUM USING POSTSUM-PARM                                   
001400     END-READ                                                             
002000     .                                                                    
010100     EJECT                                                                
010200 S11-WRITE-W55333 SECTION.                                                
010300                                                                          
010400     WRITE OP-RECORD FROM OP-AREA                                         
010600     MOVE SPACES    TO POSTSUM-TRANSTYP                                   
010700     MOVE 'W55333' TO POSTSUM-FDNAMN                                      
010800     MOVE 'W55333D2' TO POSTSUM-DDNAMN2                                   
010900     CALL POSTSUM USING POSTSUM-PARM                                      
011000     .                                                                    
030000     EJECT                                                                
030000                                                                          
010200 S12-WRITE-HEADER SECTION.                                                
010300                                                                          
010400     WRITE OP-RECORD FROM HEADER-AREA                                     
010600     MOVE SPACES    TO POSTSUM-TRANSTYP                                   
010700     MOVE 'W55333' TO POSTSUM-FDNAMN                                      
010800     MOVE 'W55333D2' TO POSTSUM-DDNAMN2                                   
010900     CALL POSTSUM USING POSTSUM-PARM                                      
011000     .                                                                    
030000     EJECT                                                                
040000 S99-ABEND SECTION.                                                       
050000                                                                          
060100     SKIP2                                                                
060200     MOVE 'S' TO POSTSUM-OPKOD                                            
061000     CALL POSTSUM USING POSTSUM-PARM                                      
070000     CALL ABEND USING RKOD-ABEND                                          
080000     .                                                                    
