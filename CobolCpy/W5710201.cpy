000100 01  OUT-W5710201.                                                        
000200*                                 MASTER INVENTORY INFO ACS               
000300     03 OUT-IDDC             PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500*                                 WAREHOUSE IDENTIFIER                    
000600     03 OUT-FLBLINDCO        PIC X.                                       
000700*                                 ANVÄND BLIND COUNT?                     
000800*                                 USE BLIND COUNT?                        
000900     03 OUT-FLNOHAND         PIC X.                                       
001000*                                 INVENTERA INAKTIVA ARTIKLAR?            
001100*                                 INVENTORY INACTIVE PARTS?               
001200     03 OUT-PRAVCOST         PIC Z(6)9.9(2).                              
001300*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
001400*                                 AVERAGE COST FOREIGN CURRENCY           
001500     03 OUT-PRAVCOST-DEV1    PIC Z(6)9.9(2).                              
001600*                                 MEDELVÄRDESKST I UTL.VAL OMG.1          
001700*                                 AVERAGE COST FOR. CUR. ROUND 1          
001800     03 OUT-PRAVCOST-DEV2    PIC Z(6)9.9(2).                              
001900*                                 MEDELVÄRDESKST I UTL.VAL OMG.2          
002000*                                 AVERAGE COST FOR. CUR. ROUND 2          
002100     03 OUT-REQTYDEV-1       PIC Z(2)9.                                   
002200*                                 KVANTITETSAVVIKELSE OMGÅNG 1            
002300*                                 QUANTITY DEVIATION ROUND 1              
002400     03 OUT-REQTYDEV-2       PIC Z(2)9.                                   
002500*                                 KVANTITETSAVVIKELSE OMGÅNG 2            
002600*                                 QUANTITY DEVIATION ROUND 2              
002700     03 OUT-SUARTAVG         PIC Z(6)9.9(2).                              
002800*                                 SUMMA AVERAGECOST                       
002900*                                 SUM OF AVERAGE COST                     
003000     03 OUT-KVINVART-INC     PIC Z(6)9.                                   
003100*                                 ANT ART SOM INVENTERAS                  
003200*                                 QTY PARTS INCL TO INVENTORY             
003300     03 OUT-KVINVART-EXCL    PIC Z(6)9.                                   
003400*                                 ANT ART SOM EJ INVENTERAS               
003500*                                 QTY PARTS EXCL FROM INVENTORY           
003600     03 OUT-KVINVART         PIC Z(6)9.                                   
003700*                                 ANTAL INVENTERADE ARTIKLAR              
003800*                                 NUMBER OF INVENTORIED PARTS             
003900     03 OUT-SUVALINV-INC     PIC Z(10)9.9(2).                             
004000*                                 INVENT. VÄRDE INKL. AV URVALET          
004100*                                 VALUE STOCK-TAKING INCL. OF SEL         
004200     03 OUT-SUVALINV-EXCL    PIC Z(10)9.9(2).                             
004300*                                 INVENT. VÄRDE EXKL. AV URVALET          
004400*                                 VALUE STOCK-TAKING EXCL OF SEL          
004500     03 OUT-SUVALINV         PIC Z(10)9.9(2).                             
004600*                                 INVENTERAT VÄRDE I URVALET              
004700*                                 VALUE STOCK-TAKING IN SELECTION         
004800     03 OUT-REQTYDEV-INC     PIC Z(2)9.9(2).                              
004900*                                 ANDEL INKLUDERAD AV URVAL               
005000*                                 SHARE INCLUDED OF SELECTION             
005100     03 OUT-REQTYDEV-EXCL    PIC Z(2)9.9(2).                              
005200*                                 ANDEL EXKLUDERAD AV URVAL               
005300*                                 SHARE EXCLUDED OF SELECTION             
005400     03 OUT-REVALDEV-INC     PIC Z(2)9.9(2).                              
005500*                                 ANDEL AV VÄRDE INCLUDERAD               
005600*                                 SHARE OF VALUE INCLUDED                 
005700     03 OUT-REVALDEV-EXCL    PIC Z(2)9.9(2).                              
005800*                                 ANDEL AV VÄRDE EXLUDERAT                
005900*                                 SHARE OF VALUE EXCLUDED                 
006000     03 OUT-TIDATETIME       PIC X(14).                                   
006100*                                 DATUM OCH TID YYYYMMDDHHMMSS            
006200*                                 DATE AND TIME YYYYMMDDHHMMSS            
006300*** END OF VILMAII-COPY LENGTH= 151 BYTES                                 
