000100 01  W0I80301.                                                            
000200     03 KDFORSKN-IN          PIC X(3).                                    
000300*                                 FÖRSÄKRANSKOD                           
000400     03 KDUPPD-FORSTEXT-IN   PIC X.                                       
000500*                                 UPPDATERINGSTYP                         
000600     03 KDFORSKN-UT          PIC X(3).                                    
000700*                                 FÖRSÄKRANSKOD                           
000800     03 KDUPPD-FORSTEXT-UT   PIC X.                                       
000900*                                 UPPDATERINGSTYP                         
001000     03 KVANTAL-TEXTRAD      PIC 9(6).                                    
001100*                                 ANTAL LÄSTA SEGMENT UNDER ROTEN         
001200     03 FLSTART-UPPDAT       PIC X.                                       
001300*                                 FÖRSTA BILDEN FÖR UPPDATERING           
001400     03 BEFORSKN-GRUPP.                                                   
001500        05 BEFORSKN          OCCURS 15 TIMES                              
001600                             PIC X(50).                                   
001700*                                 FÖRSÄKRAN VERSRAD                       
001800*** END COPY W0I80301C0  LENGTH=765                                       
