//W4754KCV JOB (510WOS39000VILM2T),'WEINBERG JACOB',                            
//             MSGCLASS=H,MSGLEVEL=(1,1),                                       
//             CLASS=N,TIME=(,30),NOTIFY=V080306                                
/*JOBPARM ROOM=PVV2,LINES=9,CARDS=0,FORMS=STD,LINECT=00                         
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
//PROC  JCLLIB ORDER=(W.IGRT.PROCLIB,W.QASE.PROCLIB)                            
//ENV  INCLUDE MEMBER=ENVQASE                                                   
//MSGOUT OUTPUT JESDS=ALL,DEFAULT=YES,                                          
//  ROOM=PVV2,DEPT='09233',ADDRESS=('VIT',                                      
//  '','')                                                                      
//*                                                                             
//RENAME  EXEC W001PTSO                                                         
//SYSTSIN DD *                                                                  
 %WRTNINP 'W.QASE.CONSTANT(W4754KCV)'                                           
//*                                                                             
//W4754K   EXEC W4754KCV,STPLIB=W.IGRT.LOAD                                     
//*                                                                             
//W4754K.SYSUDUMP DD SYSOUT=*                                                   
//W4754K.W4754KUT DD MGMTCLAS=NOBACKUP                                          
//*                                                                             
//*END     EXEC WSOPEND,PROCESS=W475J0CO                                        
