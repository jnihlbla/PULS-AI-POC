000100 01  5104-WDGX5104.                                                       
000200*                                 INVENTERING                             
000300*                                 ACS PARAMETERS                          
000400*                                 FYSISK NYCKEL: IDDC                     
000500*                                                                         
000600     03 5104-IDDC            PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 5104-DAREGDAT        PIC 9(8).                                    
001000*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
001100*                                 REGISTRATION DATE (YYYYMMDD)            
001200     03 5104-DASTADAT        PIC 9(8).                                    
001300*                                 GENERELLT STARTDATUM                    
001400*                                 GENERAL START DATE                      
001500     03 5104-DASTODAT        PIC 9(8).                                    
001600*                                 GENERELLT STOPPDATUM                    
001700*                                 GENERAL STOP DATE                       
001800     03 5104-DAUPPDAT        PIC 9(8).                                    
001900*                                 UPPDATERINGSDATUM  (ÅÅÅÅMMDD)           
002000*                                 UPDATING DATE     (YYYYMMDD)            
002100     03 5104-FLBLINDCO       PIC X.                                       
002200*                                 ANVÄND BLIND COUNT?                     
002300*                                 USE BLIND COUNT?                        
002400     03 5104-FLNOHAND        PIC X.                                       
002500*                                 INVENTERA INAKTIVA ARTIKLAR?            
002600*                                 INVENTORY INACTIVE PARTS?               
002700     03 5104-IDUSER          PIC X(8).                                    
002800*                                 ANVÄNDARENS SÄKERHETS ID                
002900*                                 USER SECURITY-IDENTITY                  
003000     03 5104-KDACS           PIC X.                                       
003100*                                 KÖRNINGSVARIANT FÖR ACS-RUTIN           
003200*                                 PROCESSING VARIANT FOR ACS RTN          
003300     03 5104-PRAVCOST        PIC S9(7)V9(2)      COMP-3.                  
003400*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
003500*                                 AVERAGE COST FOREIGN CURRENCY           
003600     03 5104-PRAVCOST-DEV1   PIC S9(7)V9(2)      COMP-3.                  
003700*                                 MEDELVÄRDESKST I UTL.VAL OMG.1          
003800*                                 AVERAGE COST FOR. CUR. ROUND 1          
003900     03 5104-PRAVCOST-DEV2   PIC S9(7)V9(2)      COMP-3.                  
004000*                                 MEDELVÄRDESKST I UTL.VAL OMG.2          
004100*                                 AVERAGE COST FOR. CUR. ROUND 2          
004200     03 5104-REQTYDEV-1      PIC S9V9(2)         COMP-3.                  
004300*                                 KVANTITETSAVVIKELSE OMGÅNG 1            
004400*                                 QUANTITY DEVIATION ROUND 1              
004500     03 5104-REQTYDEV-2      PIC S9V9(2)         COMP-3.                  
004600*                                 KVANTITETSAVVIKELSE OMGÅNG 2            
004700*                                 QUANTITY DEVIATION ROUND 2              
004800     03 5104-SUARTAVG        PIC S9(7)V9(2)      COMP-3.                  
004900*                                 SUMMA AVERAGECOST                       
005000*                                 SUM OF AVERAGE COST                     
005100*** END OF VILMAII-COPY LENGTH= 69 BYTES                                  
