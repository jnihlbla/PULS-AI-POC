000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W3W17700.                                                
000003 AUTHOR.         ANDRE KJELL.                                             
000004 DATE-WRITTEN.   2012-10-18.                                              
000005 DATE-COMPILED.                                                           
000006                                                                          
000007*    NAME:       CARPARTS.NDC.CORERECEIVINGHIST                           
000008*                                                                         
000009*    FUNCTION:                                                            
000010*        THIS IS A DRIVER PGM FOR TRANSACTION W3W177T                     
000011*                                                                         
000012*        IT TAKES CARE OF TECHNICAL DETAILS RELATED BEING CALLED          
000013*        VIA IMS-CONNECT AND CALLS SUBPROGRAM W3017710 WHICH              
000014*        CONTAINS ALL BUSINESS LOGIC FOR THIS TRANSACTION.                
000015*                                                                         
000016*        A SIMILAR DRIVER PROGRAM FOR INVOCATIONS FROM A 3270 TER-        
000017*        MINAL EXISTS - W3017700 (TRANSACTION W3T177)                     
000018*                                                                         
000019*    INDATA.                                                              
000020*        TRANSACTION: W3W177T                                             
000021*        REQUEST:     W30177I1                                            
000022*                                                                         
000023*    OUTDATA.                                                             
000024*        RESPONSE:    W30177O1                                            
000025                                                                          
000026     EJECT                                                                
000027 DATA DIVISION.                                                           
000028                                                                          
000037 WORKING-STORAGE SECTION.                                                 
000038 77  IDPGM                       PIC X(08)   VALUE 'W3W17700'.            
000039                                                                          
000040*    --- WOHK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
000041 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
000042 77  KDRC-DISPLAY                PIC Z(5).                                
000043                                                                          
000046 77  MAX-KVRADER                 PIC S9(4)   VALUE +500 COMP.             
000047                                                                          
000051                                                                          
000052*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
000053 01  GENERAL-SUBPROGRAMS.                                                 
000054     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
000055     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
000056     03  W3017710                PIC X(8)    VALUE 'W3017710'.            
000057                                                                          
000058*    --- PARAMETERS TO ABEND                                              
000059                                                                          
000060 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
000061 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
000062 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
000063     EJECT                                                                
000064*                                                                         
000065 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
000066                                                                          
000067*01  -COPY WZ01SUB                                                        
000068     EJECT                                                                
000069 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
000070                                                                          
000071 01  REQU-AREA.                                                           
000072*    03  -COPY WZ01REQU                                                   
000073*    03  -COPY W30177I1                                                   
000074     EJECT                                                                
000075 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
000076                                                                          
000077 01  RESP-AREA.                                                           
000078*    03  -COPY WZ01RESP                                                   
000079*    03  -COPY W30177O1                                                   
000080                                                                          
000081     EJECT                                                                
000082 LINKAGE SECTION.                                                         
000083 01  MSG-PCB                     PIC X.                                   
000088 01  WDM6E-PCB                   PIC X.                                   
000089 01  WDM6-PCB                    PIC X.                                   
000090                                                                          
000091     EJECT                                                                
000092 PROCEDURE DIVISION  USING MSG-PCB                                        
000095                           WDM6E-PCB                                      
000096                           WDM6-PCB.                                      
000115                                                                          
000116 MAIN SECTION.                                                            
000117     ENTRY 'DLITCBL' USING MSG-PCB                                        
000120                           WDM6E-PCB                                      
000121                           WDM6-PCB.                                      
000123                                                                          
000124     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
000125     IF SUB-KDRC = 0                                                      
000126       CALL W3017710 USING REQU-AREA RESP-AREA                            
000127                           MAX-KVRADER                                    
000128                           WDM6E-PCB WDM6-PCB                             
000134                                                                          
000135       PERFORM S02-RETURN-RESPONSE                                        
000136     END-IF                                                               
000137                                                                          
000138     MOVE ZERO TO RETURN-CODE                                             
000139     GOBACK                                                               
000140     .                                                                    
000149     EJECT                                                                
000150*    --- DISPATCHER SECTIONS                                              
000151 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
000152                                                                          
000153     MOVE 'GETARG'               TO SUB-KDFUNC                            
000154     MOVE 'CARPARTS.NDC.CORERECEIVINGHIST'  TO SUB-ADDISPABS              
000155     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
000156                                                                          
000157     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
000158                                                                          
000159     IF SUB-KDRC > 0                                                      
000160       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
000161       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
000162       DELIMITED BY SIZE INTO ERROR-TEXT                                  
000163       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
000164     END-IF                                                               
000165     .                                                                    
000166     SKIP3                                                                
000167 S02-RETURN-RESPONSE SECTION.                                             
000168                                                                          
000169     MOVE 'RETURN'                   TO SUB-KDFUNC                        
000170*    MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
000171     COMPUTE SUB-KVDLEN = LENGTH OF RESP-AREA -                           
000172           (500 - RESP-KVRADER) * LENGTH OF RESP-DATA-UT                  
000173                                                                          
000174     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
000175                                                                          
000176     IF SUB-KDRC > 0                                                      
000177       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
000178       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
000179       DELIMITED BY SIZE INTO ERROR-TEXT                                  
000180       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
000181     END-IF                                                               
000182     .                                                                    
000183     EJECT                                                                
