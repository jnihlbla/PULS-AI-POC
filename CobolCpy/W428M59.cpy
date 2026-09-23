000100 01  W428M59.                                                             
000200*                                 TABELL FÖR JUSTERING AV                 
000300*                                 FÖRSÄLJNINGSSTATISTIK                   
000400*                                 ÖVERSÄTTER DATUM TILL ÅR OCH            
000500*                                 PERIOD 3 ÅR LAGRAS I TABELLEN           
000600*                                 INNEHÅLLER ÅR -2 -1 OCH                 
000700*                                 INNEVARANDE ÅR UPPDATERAS VARJE         
000800*                                 ÅRSSKIFTE ANSVARIG  32000               
000900*                                                                         
001000     03 OVERSATTSTAB         OCCURS 24 TIMES.                             
001100        05 TIFAKT            PIC 9(6).                                    
001200*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
001300        05 TIVECKNR          PIC 9(2).                                    
001400*                                 VECKONUMMER (VV)                        
001500        05 TIAAR-PER         PIC 9(4).                                    
001600*                                 ÅR - PV-PLANERINGSPERIOD (ÅÅPP)         
001700        05 FILLER            PIC X(68).                                   
001800*** END COPY W428M59CC0  LENGTH=1920                                      
