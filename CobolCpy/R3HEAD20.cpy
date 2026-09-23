000010*** EDIT ALLOWED                                                          
000100*                     FILE TO SAP R/3 (HEADER RECORD)                     
000200*                                                                         
000300*                     ENGLISH EXPLANATION ACCORDING TO R/3                
000400*                                                                         
000500 01  HEAD-R3.                                                             
000600     03  HEAD-RECORD-TYPE          PIC X(3).                              
000700*                          200 = WITH VENDOR NO                           
000800*                          300 = WITH CUSTOMER NO                         
000900*                          600 = GL ACCOUNTS                              
001000     03  HEAD-COMPANY-CODE         PIC X(4).                              
001100*                          COMPANY                                        
001200     03  HEAD-DOCUMENT-NO          PIC X(10).                             
001300*                          DOCUMENT NUMBER                                
001400     03  HEAD-DOCUMENT-NO-REF      PIC X(16).                             
001500*                          REFERENCE DOCUMENT NUMBER                      
001600     03  HEAD-CONTROL-AREA         PIC X(4).                              
001700*                          CONTROLLING AREA                               
001800     03  HEAD-DOCUMENT-TYPE        PIC X(2).                              
001900*                          DOCUMENT TYPE                                  
002000     03  HEAD-DOCUMENT-DATE        PIC 9(8).                              
002100*                          YYYYMMDD                                       
002200     03  HEAD-POSTING-DATE         PIC 9(8).                              
002300*                          YYYYMMDD                                       
002400     03  HEAD-CURRENCY             PIC X(5).                              
002500*                          CURRENCY CODE OF ISO STD                       
002600     03  HEAD-EXCHANGE-RATE        PIC 9(4)V9(5).                         
002700*                          EXCHANGE RATE USED IN FEEDER SYSTEM            
002800     03  HEAD-TEXT                 PIC X(25).                             
002900*                          DOCUMENT HEADER TEXT                           
003000     03  HEAD-TRANSLATE-DATE       PIC 9(8).                              
003100*                          YYYYMMDD - TRANSLATION DATE                    
003200     03  HEAD-INT-COMPANY-NUMBER   PIC X(16).                             
003300*                          INTERCOMPANY NUMBER                            
003400     03  HEAD-TRADING-PARTNER-BA   PIC X(4).                              
003500*                          TRADING PARTNERS BUSINESS AREA                 
003600     03  HEAD-EXCHANGE-TYPE        PIC X(4).                              
003700*                          EXCHANGE RATE TYPE                             
003710     03  HEAD-POSTING-PERIOD       PIC X(2).                              
003720*                          POSTING  PERIOD                                
003730     03  HEAD-EXCHANGE-TOFACT      PIC X(5).                              
003740*                          EXCHANGE RATE TO                               
003750     03  HEAD-EXCHANGE-FRFACT      PIC X(5).                              
003760*                          EXCHANGE RATE FROM                             
003770     03  HEAD-REVERSAL-REAS        PIC X(2).                              
003780*                          REVERSAL REASON                                
003790     03  HEAD-REVERSAL-DATE        PIC X(8).                              
003791*                          YYYYMMDD REVERSAL DATE                         
003800*** END OF VILMAII-COPY LENGTH= 148 OLD LENGTH= 148                       
