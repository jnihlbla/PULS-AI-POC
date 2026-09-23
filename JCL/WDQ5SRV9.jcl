//WDQ5SRV9 JOB (650W0020200WDQ5SRV9,W100),'RTN W010V9',                         
//             USER=?,PASSWORD=?,                                               
//             CLASS=L                                                          
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*JOBPARM FORMS=1800,LINECT=0                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//*                                                                             
//IDCAMS  EXEC WIDCAMS,CLUSTER=WDQ5CLU                                          
//*                                                                             
//WDQ5    EXEC WG01REL,                                                         
//             DBD=WDQ5                                                         
//REL.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(NEW,PASS,DELETE),                        
//             SPACE=(4096,(54000,1800),RLSE),                                  
//             DCB=BUFNO=10,DATACLAS=MVOL                                       
//REL.DBORELD1 DD DSN=WG01.UNLO.WDQ5K(+0),DISP=SHR                              
//REL.WDQ5K DD DSN=WG01.QASE.WDQ5K,DISP=SHR                                     
//*                                                                             
//REL.IDIPARM  DD  DISP=SHR,DSN=F1IM00.IMSTOOL.PARMLIB                          
//*                                                                             
//IDCAMS  EXEC WIDCAMS                                                          
//DD1      DD DSN=WG01.QASE.WDQ5K,DISP=SHR                                      
//SYSIN    DD DSN=W.QASE.CONSTANT(WDQ5ACLU),DISP=SHR                            
//*                                                                             
//        EXEC WG01SIU                                                          
//SIU.DFSURWF1 DD DSN=&&DFSURWF1,DISP=(OLD,PASS,DELETE)                         
//SIU.WDQ5AK DD DSN=WG01.QASE.WDQ5AK,DISP=SHR                                   
//SIU.DBOCTRL   DD *                                                            
* CONVERTED BY CA TECHNOLOGIES 24 FEB 2022 19:42:48                             
                                                                                
  FUNCTION=INDEXCREATE,DBDNAME=WDQ5,INDD=DFSURWF1,ICNEEDED=OFF,                 
  IIRRECORD=NO,DBRC=YES                                                         
                                                                                
/*                                                                              
//SOP     EXEC WSOPEND,PROCESS=WDQ5SRV9                                         
