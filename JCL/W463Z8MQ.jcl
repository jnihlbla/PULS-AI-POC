//W463Z8MQ JOB (670W4630100W463Z8MQ,W100),'RTN W463D6',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//EMPTYT  EXEC WEMPTST,DSIN=W463.W463D6.W463PMQ(+0)                             
//*                                                                             
//     IF (EMPTYT.T.RC = 0) THEN                                                
//* DIR BUSINESS, BILLINGTRANSAR TILL PRICE-FOR ALL MARKETS                     
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W463.W463D6.W463PMQ(+0)                                     
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.PULS.DIRECTBUSINESSBILLPRICE                                
/*                                                                              
//     ENDIF                                                                    
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W463Z8MQ                                         
