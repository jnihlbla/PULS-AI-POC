//W463Z5MQ JOB (670W4630100W463Z5MQ,W100),'RTN W463D6',                         
//             CLASS=K,USER=?,PASSWORD=?                                        
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE   XEQ LOCAL                                                             
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//EMPTYT  EXEC WEMPTST,DSIN=W463.W463D6.W4636MQ(+0)                             
//*                                                                             
//     IF (EMPTYT.T.RC = 0) THEN                                                
//* DIR BUSINESS, BILLINGTRANSAR TILL VIPS-FOR ALL MARKETS                      
//WQSEN   EXEC WZ11P023,                                                        
//             DSIN=W463.W463D6.W4636MQ(+0)                                     
//WZ1123.SYSIN DD *                                                             
¤ADDISPABS CARPARTS.VIPS.DIRECTBUSINESSBILLING                                  
/*                                                                              
//     ENDIF                                                                    
//*                                                                             
//SOPEND  EXEC WSOPEND,PROCESS=W463Z5MQ                                         
