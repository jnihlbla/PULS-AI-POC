000100 01  W0O80301.                                                            
000200     03 IDTRANS              PIC X(4).                                    
000300*                                 TRANSAKTIONSIDENTITET                   
000400     03 MESSAGE-RAD1         PIC X(40).                                   
000500*                                 MEDDELANDEFÄLT PÅ RAD 1                 
000600     03 KDFORSKN-IN          PIC X(3).                                    
000700*                                 FÖRSÄKRANSKOD                           
000800     03 KDUPPD-FORSTEXT-IN   PIC X.                                       
000900*                                 UPPDATERINGSTYP                         
001000     03 KDFORSKN-UT          PIC X(3).                                    
001100*                                 FÖRSÄKRANSKOD                           
001200     03 KDUPPD-FORSTEXT-UT   PIC X.                                       
001300*                                 UPPDATERINGSTYP                         
001400     03 KVANTAL-TEXTRAD      PIC 9(6).                                    
001500*                                 ANTAL LÄSTA SEGMENT UNDER ROTEN         
001600     03 FLSTART-UPPDAT       PIC X.                                       
001700*                                 FÖRSTA BILDEN FÖR UPPDATERING           
001800     03 BEFORSKN-GRUPP.                                                   
001900        05 BEFORSKN          OCCURS 15 TIMES                              
002000                             PIC X(50).                                   
002100*                                 FÖRSÄKRAN VERSRAD                       
002200     03 MESSAGE-RAD23        PIC X(79).                                   
002300*                                 MEDDELANDEFÄLT PÅ RAD 23                
002400*** END COPY W0O80301C0  LENGTH=888                                       
